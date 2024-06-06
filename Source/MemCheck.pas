(*
MemCheck: the ultimate memory troubles hunter
Originally Created by: Jean Marc Eber & Vincent Mahon, Société Générale, INFI/SGOP/R&D

Version RS10.2 Tokyo revised by J Lin 27 July 2023

Contact...
	Vincent.Mahon@free.fr
	http://v.mahon.free.fr/pro/freeware/memcheck

Mail address:
	Tour Société Générale
	Sgib/Sgop/R&D
	92987 Paris - La Défense cedex
	France


General information...
MemCheck logs information each time memory is allocated, reallocated or freed. When the program ends, information about memory problems is provided in a log file
and exceptions are raised at problematic points.

Basic use...
Set the MemCheckLogFileName option. Call MemChk when you want to start the memory monitoring. Nothing else to do !
When your program terminates and the finalization is executed, MemCheck will report the problems. This is the behaviour you'll obtain if you change no option in MemCheck.

Features...
- List of memory spaces not deallocated, and raising of EMemoryLeak exception at the exact place in the source code
- Call stack at allocation time. User chooses to see or not to see this call stack at run time (using ShowCallStack), when a EMemoryLeak is raised.
- Tracking of virtual method calls after object's destruction (we change the VMT of objects when they are destroyed)
- Tracking of method calls on an interface while the object attached to the interface has been destroyed
- Checking of writes beyond end of allocated blocks (we put a marker at the end of a block on allocation)
- Fill freed block with a byte (this allows for example to set fields of classes to Nil, or buffers to $FF, or whatever)
- Detect writes in deallocated blocks (we do this by not really deallocating block, and checking them on end - this can be time consuming)
- Statistics collection about objects allocation (how many objects of a given class are created ?)
- Time stamps can be indicated and will appear in the output

Options and parameters...
- You can specify the log files names (MemCheckLogFileName)
- It is possible to tell MemCheck that you are instanciating an object in a special way - See doc for  CheckForceAllocatedType
- Clients can specify the depth of the call stack they want to store (StoredCallStackDepth)

Warnings...
- MemCheck is based on a lot of low-level hacks. Some parts of it will not work on other versions of Delphi
  without being revisited (as soon as System has been recompiled, MemCheck is very likely to behave strangely,
  because for example the address of InitContext will be bad).
- Some debugging tools exploit the map file to return source location information. We chose not to do that, because
  we think the way MemCheck raises exceptions at the good places is better. It is still possible to use "find error" in Delphi.
- Memcheck is not able to report accurate call stack information about a leak of a class which does not redefine
  its constructor. For example, if an instance of TStringList is never deallocated, the call stack MemCheck will
  report is not very complete. However, the leak is correctly reported by MemCheck.

A word about uses...
 Since leaks are reported on end of execution (finalization of this unit), we need as many finalizations to occur before memcheck's, so that if some memory
 is freed in these finalizations, it is not erroneously reported as leak. In order to finalize MemCheck as late as possible, we use a trick to change the
 order of the list of finalizations. Other memory managing products which are available (found easily on the internet) do not have this problem because they
 just rely on putting the unit first in the DPR; but this is not safe without a build all. In MemCheck we absolutely need to use two units: SysUtils and Windows.
 Then, I decided in MemCheck 2.54 to use the unit Classes because I think it will lead to much simpler code. We also use two units which we can use without risk
 since they dont have a finalization: Math and SyncObjs.

An analysis of the uses clauses of these five units shows that in fact MemCheck uses indirectly the following units:
  Math, Classes, Typinfo, Consts, Variants, VaRUtils, SysUtils, ActiveX, Messages, SysConst, Windows, SyncObjs, System, SysInit and Types.

Of these, only Classes, Variants, System and SysUtils have a finalization section. I checked and it is not possible to have a leak reported by MemCheck which
is not correct because the memory would have been freed by one of these finalizations.
In ChangeFinalizationsOrder only these four units are finalized after MemCheck (I could have decided for the fifteen, but this would be more work, and I know it is useless).

*)

unit MemCheck;
{$A+}
{$H+}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

                                              interface
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uses
	Windows,  Classes,	Math,	SyncObjs, Character,
	SysUtils;						  //Because of this uses, SysUtils must be finalized after MemCheck - Which is necessary anyway because SysUtils calls DoneExceptions in its finalization
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

const
	StoredCallStackDepth = 27;	//Size of the call stack we store when GetMem is called, must be an EVEN number
	TmpCallStackDepth = 5;
	TObjectVirtualMethodNames: array[1..8] of string = ('SafeCallException', 'AfterConstruction', 'BeforeDestruction', 'Dispatch', 'DefaultHandler', 'NewInstance', 'FreeInstance', 'Destroy');
	AddressOfNewInstance: pointer = @TObject.NewInstance;
	AddressOfNew : pointer = @MoveArray; //+ $60
	AddressOfTObjectCreate: pointer = @TObject.Create;
  AddressOfClassCreate : Pointer = @TObject.FieldAddress;//ClassCreate is next to it;
  AddressOfInterfacedObj : Pointer = @TInterfacedObject.NewInstance;
  AddressOfTOwnedCollectionCreate : Pointer = @TOwnedCollection.Create;

	MaxNbSupportedVMTEntries = 200; //Don't change this number, its a Hack! jm
 	MaxListSize = MaxInt div 16 - 1;

type TCallStack = array[0..StoredCallStackDepth - 1] of Pointer;
type TCallStackLst = array[0..TmpCallStackDepth -1] of Pointer;
type TKindOfMemory = (MClass, MUser, MReallocedUser);
	{MClass means the block carries an object
	MUser means the block is a buffer of unknown type (in fact we just know this is not an object)
	MReallocedUser means this block was reallocated}


type
	PObjectsArray = ^TObjectsArray;
	TObjectsArray = array[0..MaxListSize] of TObject;

	PStringsArray = ^TStringsArray;
	TStringsArray = array[0..99999999] of string;	{Used to simulate string lists}

	PIntegersArray = ^TIntegersArray;
	TIntegersArray = array[0..99999999] of integer; {Used to simulate lists of integer}

type
	PMemoryBlocHeader = ^TMemoryBlocHeader;
	TMemoryBlocHeader = record
		{
		This is the header we put in front of a memory block
		For each memory allocation, we allocate "size requested + header size + footer size" because we keep
    information inside the memory zone.
		Therefore, the address returned by GetMem is: [the address we get from OldMemoryManager.GetMem] + HeaderSize.

		. DestructionAdress: an identifier telling if the bloc is active or not
      (when FreeMem is called we do not really free the mem).
		  Nil when the block has not been freed yet; otherwise, contains the address of the caller of
      the destruction. This will be useful  for reporting errors such as "this memory has already been freed,
      at address XXX".
		. PreceedingBlock: link of the linked list of allocated blocs
		. NextBlock: link of the linked list of allocated blocs
		. KindOfBlock: is the data an object or unknown kind of data (such as a buffer)
		. VMT: the classtype of the object
		. CallerAddress: an array containing the call stack at allocation time
		. AllocatedSize: the size allocated for the user (size requested by the user)
		. MagicNumber: an integer we use to recognize a block which was allocated using our own allocator
		}
		DestructionAdress: Pointer;
		PreceedingBlock: Pointer;
		NextBlock: Pointer;
		KindOfBlock: TKindOfMemory;
		VMT: TClass;
		CallerAddress: TCallStack;
		AllocatedSize: integer;		 //this is an integer because the parameter of GetMem is an integer
		LastTimeStamp: integer;		 //-1 means no time stamp
		NotUsed: Cardinal;			  //Because Size of the header must be a multiple 8
		MagicNumber: Cardinal;
	end;

	PMemoryBlockFooter = ^TMemoryBlockFooter;
	TMemoryBlockFooter = Cardinal;	{This is the end-of-bloc marker we use to check that the user did not write beyond the allowed space}

	EMemoryLeak = class(Exception);
	EStackUnwinding = class(EMemoryLeak);
	EBadInstance = class(Exception);	{This exception is raised when a virtual method is called on an object which has been freed}
	EFreedBlockDamaged = class(Exception);
	EInterfaceFreedInstance = class(Exception);	{This exception is raised when a method is called on an interface whom object has been freed}

	VMTTable = array[0..MaxNbSupportedVMTEntries] of pointer;
	pVMTTable = ^VMTTable;
	TMyVMT = record
		A: array[0..19] of byte;
		B: VMTTable;
	end;

	ReleasedInstance = class
		procedure RaiseExcept;
		procedure InterfaceError; stdcall;
		procedure Error; virtual;
	end;

	TFieldInfo = class
		OwnerClass: TClass;
		FieldIndex: integer;

		constructor Create(const TheOwnerClass: TClass; const TheFieldIndex: integer);
	end;

type
	TIntegerBinaryTree = class
	protected
		fValue: Cardinal;
		fBigger: TIntegerBinaryTree;
		fSmaller: TIntegerBinaryTree;

		class function StoredValue(const Address: Cardinal): Cardinal;
		constructor _Create(const Address: Cardinal);
		function _Has(const Address: Cardinal): Boolean;
		procedure _Add(const Address: Cardinal);
		procedure _Remove(const Address: Cardinal);

	public
		function Has(const Address: Cardinal): Boolean;
		procedure Add(const Address: Cardinal);
		procedure Remove(const Address: Cardinal);

		property Value: Cardinal read fValue;
	end;

	PCardinal = ^Cardinal;

type
	TAddressToLine = class
	public
		Address: Cardinal;
		Line: Cardinal;

		constructor Create(const AAddress, ALine: Cardinal);
	end;

	PAddressesArray = ^TAddressesArray;
	TAddressesArray = array[0..MaxInt div 16 - 1] of TAddressToLine;

	TUnitDebugInfos = class
	public
		Name: string;
		Addresses: array of TAddressToLine;

		constructor Create(const AName: string; const NbLines: Cardinal);

		function LineWhichContainsAddress(const Address: Cardinal): string;
	end;

	TRoutineDebugInfos = class
	public
		Name: AnsiString;
		StartAddress: Cardinal;
		EndAddress: Cardinal;

		constructor Create(const AName: AnsiString; const AStartAddress: Cardinal; const ALength: Cardinal);
	end;

type UntTblAry = array  of PackageUnitEntry;



//----------------------------------------------
procedure MemChk; //Activates MemCheck and resets the allocated blocks stack. Warning: the old stack is lost ! - It is the client's duty to commit the
                  // releasable blocks by calling CommitReleases(AllocatedBlocks)
procedure UnMemChk; //sets back the memory manager that was installed before MemChk was called If MemCheck is not active, this does not matter. The default delphi memory manager is set.
                    //You should be very careful about calling this routine and know exactly what it does (see the FAQ on the web site)
procedure CommitReleases;  //really releases the blocks
procedure AddTimeStampInformation(const I: string);//Logs the given information as associated with the current time stamp Requires that MemCheck is active
procedure LogSevereExceptions(const WithVersionInfo: string);//Activates the exception logger
function  MemoryBlockCorrupted(P: Pointer): Boolean;
function  BlockAllocationAddress(P: Pointer): Pointer; //The address at which P was allocated If MemCheck was not running when P was allocated (ie we do not find our magic number), we return $00000000
function  IsMemCheckActive: boolean; //Is MemCheck currently running ? ie, is the current memory manager memcheck's ?
function  GetDebugInfoAt(const TheAddress: Cardinal): string;
procedure SetDispl;
procedure FillCallStack(var St: TCallStack; const NbLevelsToExclude: integer); //Fills St with the call stack
function  CallStackTextualRepresentation(const S: TCallStack; const LineHeader: string): string; //Will contain CR/LFs

//----------------------------------------------
function LeakTrackingGetMem(Size: NativeInt): Pointer;
function LeakTrackingReallocMem(P: Pointer; Size: NativeInt): Pointer;
function LeakTrackingFreeMem(P: Pointer): Integer;
procedure GoThroughAllocatedBlocks;
function RtlCaptureStackBackTrace(FramesToSkip: ULONG; FramesToCapture: ULONG; BackTrace: Pointer;  BackTraceHash: PULONG): USHORT; stdcall; external 'kernel32.dll';
procedure GetCallStacks(var Stack: TCallStack; FramesToSkip: Integer);
procedure GetCallStackLst(var Stack: TCallStackLst; FramesToSkip: Integer);
function CallerIsNewAnsiString : boolean;
function CallerIsNewUniString : boolean;
function CallerIsNewInstance : boolean;
function CallerIsDynamicArrayAllocation : boolean;
function CallerOfReallocMem: pointer;

Type TShowStackLst = Function (var CallerLst : TCallStack; MapFName : String; CallersNum : Integer; ExeOffset : uInt64; ShowPos : Pointer) : Boolean;


//----------------------------------------------

var
	BlockSzLmt: Integer = 0;	       //0 = all blocks to be deallocated. MaxInt = never deallocated. When blocks are not deallocated, MemCheck can give information about when the second deallocation occured
	ShowLogFile: Boolean = True;
  ignoreLeakMsg : boolean = True;  //ignore mem leak message for customers
 	ShowCallStack: Boolean;	         //When we show an allocated block, should we show the call stack that went to the allocation ? Set to false
                          	       //before each block. The usual way to use this is calling Evaluate/Modify just after an EMemoryLeak was raised
  UnitTbls : UntTblAry;
  memchkPtr : Pointer;
	UnitsInfo: PackageInfo;


	DisplOfExe: uInt64;	//Displ is the displacement of the code in the executable file. The code in SetDispl was written by Juan Vidal Pich
  MapFileName : String = '';  //GR_Orderpad.map

  TmpCallers  : TCallStackLst;

  StartingSum : Boolean = False;
  CurMemUsage : Integer = 0;
  SumMemFreed : Integer = 0;
                                          implementation
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
uses Forms;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
var AddressOfTCustomFormCreate, AddressOfTGlassFrameCreate : pointer;
const
  StackFrmToSkip = 2;
	EndOfBlock: Cardinal = $FFFFFFFA;
	Magic: Cardinal = $FFFFFFFF;
	FreedMagic: Cardinal = $EE99988E;
	DeallocateSpecifiedClass = False;
	DeallocationClass: TClass = TObject;
	{used only when BlocksToShow[MClass] is True - eg If InstancesConformingTo = TList, only blocks allocated for instances	of TList and its heirs will be shown}
	InstancesConformingToForReporting: TClass = TObject;
	{used only when BlocksToShow[MClass] is True - eg If InstancesConformingTo = TList, only blocks allocated for instances	of TList and its heirs will be shown}

	NoDebugInfo = ' ';//'(no debug info)';
	MemCheckLogFileNameSuffix = '_MemChk.log';

	(**************** MEMCHECK OPTIONS ********************)
	DanglingInterfacesVerified = False;
	{When an object is destroyed, should we fill the interface VMT with a special value which
	will allow tracking of calls to this interface after the object was destroyed - This incompatible with CheckWipedBlocksOnTermination, so you have to choose}

	WipeOutMemoryOnFreeMem = True;
	{This is about what is done on memory freeing:
	- for objects, this option replaces the VMT with a special one which will raise exceptions if a virtual method is called
	- for other memory kinds, this will fill the memory space with the char below}
	CharToUseToWipeOut: Ansichar = #0; 	//I choose #0 because this makes objet fields Nil, which is easier to debug. Tell me if you have a better idea !

	CheckWipedBlocksOnTermination = True and WipeOutMemoryOnFreeMem and not (DanglingInterfacesVerified);
	{When iterating on the blocks (in OutputAllocatedBlocks), we check for every block which has been deallocated that it is still
	filled with CharToUseToWipeOut.
	Warning: this is VERY time-consuming
	This is meaningful only when the blocks are wiped out on free mem
	This is incompatible with dangling interfaces checking}
	DoNotCheckWipedBlocksBiggerThan = 4000;

	CollectStatsAboutObjectAllocation = False;
	{Every time FreeMem is called for allocationg an object, this will register information about the class instanciated:
	class name, number of instances, allocated space for one instance
	Note: this has to be done on FreeMem because when GetMem is called, the VMT is not installed yet and we can not know
	this is an object}

	KeepMaxMemoryUsage = CollectStatsAboutObjectAllocation; //Will report the biggest memory usage during the execution

	ComputeMemoryUsageStats = False; //Outputs the memory usage along the life of the execution. This output can be easily graphed, in excel for example
	MemoryUsageStatsStep = 5;	//Meaningful only when ComputeMemoryUsageStats	When this is set to 5, we collect information for the stats every 5 call to GetMem, unless size is bigger than StatCollectionForce
	StatCollectionForce = 1000;
	BlocksToShow: array[TKindOfMemory] of Boolean = (true, true, true);	//eg if BlocksToShow[MClass] is True, the blocks allocated for class instances will be shown
	CheckHeapStatus = False;	// Checks that the heap has not been corrupted since last call to the memory manager.  Warning: VERY time-consuming
	IdentifyObjectFields = False;
	IdentifyFieldsOfObjectsConformantTo: TClass = Tobject;

	MaxLeak = 1000;	//This option tells to MemCheck not to display more than a certain quantity of leaks, so that the finalization phase does not take too long
	UseDebugInfos = True;  	//Should use the debug informations which are in the executable ?
	RaiseExceptionsOnEnd = true; //Should we use exceptions to show memory leak information ?

	NotepadApp = 'notepad.exe'; //The application launched to show the log file


//------------------------------------------------------------------
var
  CurrentlyAllocatedBlocksTree: TIntegerBinaryTree;

	TimeStamps: PStringsArray = nil;  //Allows associating a string of information with a time stamp
	TimeStampsCount: integer = 0;	    //Number of time stamps in the array
	TimeStampsAllocated: integer = 0;	//Number of positions available in the array

	Routines: array of TRoutineDebugInfos;
	RoutinesCount: integer;
	Units: array of TUnitDebugInfos;
	UnitsCount: integer;
	OutputFileHeader: string = 'MemCheck version RS10.2'#13#10;
	HeapStatusSynchro : TSynchroObject;

	FreedInstance: PAnsiChar;
	BadObjectVMT: TMyVMT;
	BadInterfaceVMT: VMTTable;
	GIndex: Integer;

	LastBlock: PMemoryBlocHeader;

	MemCheckActive: boolean = False;	{Is MemCheck currently running ?	ie, is the current memory manager memcheck's ?}
	MemCheckInitialized: Boolean = False;	{Has InitializeOnce been called ?	This variable should ONLY be used by InitializeOnce and the finalization}

   {*** arrays for stats ***}
	AllocatedObjectsClasses: array of TClass;
	NbClasses: integer = 0;

	AllocatedInstances: PIntegersArray = nil; {instances counter}
	AllocStatsCount: integer = 0;
	StatsArraysAllocatedPos: integer = 0;
	{This is used to display some statistics about objects allocated. Each time an object is allocated, we look if its
	class name appears in this list. If it does, we increment the counter of class' instances for this class;
	if it does not appear, we had it with a counter set to one.}

	MemoryUsageStats: PIntegersArray = nil; {instances counter}
	MemoryUsageStatsCount: integer = 0;
	MemoryUsageStatsAllocatedPos: integer = 0;
	MemoryUsageStatsLoop: integer = -1;

	SevereExceptionsLogFile: Text;	{This is the log file for exceptions}

	OutOfMemory: EOutOfMemory;	// Because when we have to raise this, we do not want to have to instanciate it (as there is no memory available)

	HeapCorrupted: Exception;

	NotDestroyedFields: PIntegersArray = nil;
	NotDestroyedFieldsInfos: PObjectsArray = nil;
	NotDestroyedFieldsCount: integer = 0;
	NotDestroyedFieldsAllocatedSpace: integer = 0;

	LastHeapStatus: THeapStatus;

	MaxMemoryUsage: Integer = 0;	// see KeepMaxMemoryUsage
	OldMemoryManager: TMemoryManagerEx;	//Set by the MemChk routine


//-----------------------------------------------------
function BlockAllocationAddress(P: Pointer): Pointer;
var	Block: PMemoryBlocHeader;
begin
	Block := PMemoryBlocHeader(PAnsiChar(P) - SizeOf(TMemoryBlocHeader));

	if Block.MagicNumber = Magic then
		Result := Block.CallerAddress[0]
	else
		Result := nil
end;

//-----------------------------------------------------
procedure UpdateLastHeapStatus;
begin
	LastHeapStatus := GetHeapStatus;
end;

//-----------------------------------------------------
function HeapStatusesDifferent(const Old, New: THeapStatus): boolean;
begin
	Result :=
		(Old.TotalAddrSpace <> New.TotalAddrSpace) or
		(Old.TotalUncommitted <> New.TotalUncommitted) or
		(Old.TotalCommitted <> New.TotalCommitted) or
		(Old.TotalAllocated <> New.TotalAllocated) or
		(Old.TotalFree <> New.TotalFree) or
		(Old.FreeSmall <> New.FreeSmall) or
		(Old.FreeBig <> New.FreeBig) or
		(Old.Unused <> New.Unused) or
		(Old.Overhead <> New.Overhead) or
		(Old.HeapErrorCode <> New.HeapErrorCode) or
		(New.TotalUncommitted + New.TotalCommitted <> New.TotalAddrSpace) or
		(New.Unused + New.FreeBig + New.FreeSmall <> New.TotalFree)
end;

//-----------------------------------------------------
class function TIntegerBinaryTree.StoredValue(const Address: Cardinal): Cardinal;
begin
	Result := Address shl 16;
	Result := Result or (Address shr 16);
	Result := Result xor $AAAAAAAA;
end;

//-----------------------------------------------------
constructor TIntegerBinaryTree._Create(const Address: Cardinal);
begin	//We do not call inherited Create for optimization
	fValue := Address
end;

//-----------------------------------------------------
function TIntegerBinaryTree.Has(const Address: Cardinal): Boolean;
begin
	Result := _Has(StoredValue(Address));
end;

//-----------------------------------------------------
procedure TIntegerBinaryTree.Add(const Address: Cardinal);
begin
	_Add(StoredValue(Address));
end;

//-----------------------------------------------------
procedure TIntegerBinaryTree.Remove(const Address: Cardinal);
begin
	_Remove(StoredValue(Address));
end;

//-----------------------------------------------------
function TIntegerBinaryTree._Has(const Address: Cardinal): Boolean;
begin
	if fValue = Address then
		Result := True
	else
		if (Address > fValue) and (fBigger <> nil) then
			Result := fBigger._Has(Address)
		else
			if (Address < fValue) and (fSmaller <> nil) then
				Result := fSmaller._Has(Address)
			else
				Result := False
end;

//-----------------------------------------------------
procedure TIntegerBinaryTree._Add(const Address: Cardinal);
begin
	Assert(Address <> fValue, 'TIntegerBinaryTree._Add: already in !');

	if (Address > fValue) then
		begin
			if fBigger <> nil then
				fBigger._Add(Address)
			else
				fBigger := TIntegerBinaryTree._Create(Address)
		end
	else
		begin
			if fSmaller <> nil then
				fSmaller._Add(Address)
			else
				fSmaller := TIntegerBinaryTree._Create(Address)
		end
end;

//-----------------------------------------------------
procedure TIntegerBinaryTree._Remove(const Address: Cardinal);
var
	Owner, Node: TIntegerBinaryTree;
	NodeIsOwnersBigger: Boolean;
	Middle, MiddleOwner: TIntegerBinaryTree;
begin
	Owner := nil;
	Node := CurrentlyAllocatedBlocksTree;

	while (Node <> nil) and (Node.fValue <> Address) do
		begin
			Owner := Node;

			if Address > Node.Value then
				Node := Node.fBigger
			else
				Node := Node.fSmaller
		end;

	Assert(Node <> nil, 'TIntegerBinaryTree._Remove: not in');

	NodeIsOwnersBigger := Node = Owner.fBigger;

	if Node.fBigger = nil then
		begin
			if NodeIsOwnersBigger then
				Owner.fBigger := Node.fSmaller
			else
				Owner.fSmaller := Node.fSmaller;
		end
	else
		if Node.fSmaller = nil then
			begin
				if NodeIsOwnersBigger then
					Owner.fBigger := Node.fBigger
				else
					Owner.fSmaller := Node.fBigger;
			end
		else
			begin
				Middle := Node.fSmaller;
				MiddleOwner := Node;

				while Middle.fBigger <> nil do
					begin
						MiddleOwner := Middle;
						Middle := Middle.fBigger;
					end;

				if Middle = Node.fSmaller then
					begin
						if NodeIsOwnersBigger then
							Owner.fBigger := Middle
						else
							Owner.fSmaller := Middle;

						Middle.fBigger := Node.fBigger
					end
				else
					begin
						MiddleOwner.fBigger := Middle.fSmaller;

						Middle.fSmaller := Node.fSmaller;
						Middle.fBigger := Node.fBigger;

						if NodeIsOwnersBigger then
							Owner.fBigger := Middle
						else
							Owner.fSmaller := Middle
					end;
			end;

	Node.Destroy;
end;

//-----------------------------------------------------
constructor TFieldInfo.Create(const TheOwnerClass: TClass; const TheFieldIndex: integer);
begin
	inherited Create;

	OwnerClass := TheOwnerClass;
	FieldIndex := TheFieldIndex;
end;


//Checked 24 July 23 For RS10.2
//-----------------------------------------------------
function CallerOfCaller: pointer;	//with stack frames !
asm
  {$IFDEF WIN32} //CPUX86
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
	mov eax, [EBP]
	cmp eax, ebp
	jb @@EndOfStack
	mov eax, [ebp + 8]
	sub eax, 5
	ret
	@@EndOfStack:
	mov eax, $FFFF
  {$ENDIF}
end;

//Checked 24 July 23 For RS10.2
//-----------------------------------------------------
function Caller: pointer;
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
	mov eax, [ebp + 4]
	sub eax, 4
	ret
	@@EndOfStack:
	mov eax, $FFFF
end;


//Checked 27 July 23 For RS10.2
//-----------------------------------------------------
function CallerOfReallocMem: pointer;	//System._ReallocMem has no stack frame
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
	mov eax, [EBP + 12]
	sub eax, 5
	ret
	@@EndOfStack:
	mov eax, $FFFF
end;




//Works only with stack frames - Without, St contains correct info, but is not as deep as it should
//I just don't know a general rule for walking the stack when they are not there
//----------------------------------------------------------------------------------------------------------------
procedure FillCallStack(var St: TCallStack; const NbLevelsToExclude : integer);
var
	StackStart: Pointer;
	StackMax: Pointer;	//the stack can never go beyond - http://msdn.microsoft.com/library/periodic/period96/S2CE.htm
	CurrentFrame: Pointer;
	Count, SkipCount: integer;
begin
	FillChar(St, SizeOf(St), Byte(0));
	asm
		mov EAX, FS:[4]
		mov StackMax, EAX
		mov StackStart, EBP
	end;

	CurrentFrame:= StackStart;
	Count:= 0;
	SkipCount:= 0;
	while (longint(CurrentFrame) >= longint(StackStart)) and (longint(CurrentFrame) < longint(StackMax)) and (Count <= StoredCallStackDepth - 1) do begin
    if SkipCount >= NbLevelsToExclude then begin
      St[Count]:= Pointer(PInteger(longint(CurrentFrame) + 4)^ - 4);   //Ret address is stored at address of Current frame + 4
      Count:= Count + 1;
    end;
    CurrentFrame:= Pointer(PInteger(CurrentFrame)^);
    SkipCount:= SkipCount + 1;
  end;

end;


//------------------------------------------------------------------------------
{unit System :
class function TObject.NewInstance: TObject;
begin
  Result := InitInstance(_GetMem(InstanceSize));   //_GetMen = LeakTrackingGetMem
  retAddress:
end;

  LeakTrackingGetMem :
  asm
    Push ebp;                               //Before push, at stack pointer esp - 4 : retAddress
    move ebp, esp;                          //ebp = current stack pointer. Now at esp - 8 : retAddress
  end;
}

//Checked 27 July 23 For RS10.2
//------------------------------------------------------------------------------
function CallerIsNewInstance: boolean;	//TObject.NewInstance has no stack frame
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@no
	mov eax, [ebp + 8]                //TObject.NewInstance return address
	sub eax, $0F                      //Distance = retAddress - TObject.NewInstance = 15 ($0F).
	cmp eax, AddressOfNewInstance
	je @@yes
	@@no:
	mov eax, 0
	ret
	@@yes:
	mov eax, 1
end;



//Tells the address of the caller of FreeInstance from LeakTrackingFreeMem
//Checked 27 July 23 For RS10.2
//------------------------------------------------------------------------------
function CallerOfFreeInstance: pointer;
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
	mov eax, [EBP + 16]
	sub eax, 3
	ret
	@@EndOfStack:
	mov eax, $FFFF
end;

//Tells the address of the caller of System._FreeMem from LeakTrackingFreeMem
//Checked 27 July 23 For RS10.2
//---------------------------------------------------------------------------
function CallerOf_FreeMem: pointer;
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@EndOfStack
	mov eax, [EBP + 8]
	sub eax, 5
	ret
	@@EndOfStack:
	mov eax, $FFFF
end;


//Checked 27 July 23 For RS10.2
//------------------------------------------------------
function CallerIsNewAnsiString: boolean;
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@no
	mov eax, [ebp + 8]         //J Lin : ret address of CALL _GetMem in System._NewAnsiString
	sub eax, $14
	cmp eax, offset System.@NewAnsiString
	je @@yes
	@@no:
	mov eax, 0
	ret
	@@yes:
	mov eax, 1
end;


//Checked 27 July 23 For RS10.2
//----------------------------------------
function CallerIsNewUniString: boolean;
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@no
	mov eax, [ebp + 8]    //ret address of CALL _GetMem in System.@NewUnicodeString
	sub eax, 19           //JLin : ret address (next asm line) of CALL _GetMem - System.@NewUnicodeString Address = 19
	cmp eax, offset System.@NewUnicodeString
	je @@yes
	@@no:
	mov eax, 0
	ret
	@@yes:
	mov eax, 1
end;


//Checked 27 July 23 For RS10.2
//------------------------------------------------
function CallerIsDynamicArrayAllocation: boolean;
asm
	cmp ebp, 0	//this can happen when there are no stack frames
	je @@no
	mov eax, [EBP + $44]         //get ret address($0040D392) of DynArraySetLength in _DynArraySetLength
	sub eax, $0A
	cmp eax, offset System.@DynArraySetLength
	je @@yes
	@@no:
	mov eax, 0
	ret
	@@yes:
	mov eax, 1
end;

//------------------------------------------------
procedure ReleasedInstance.RaiseExcept;
var
	t: TMemoryBlocHeader;
	i: integer;
	FeedBackStr, AlloAtStr, DetroyAtStr: string;
begin
	t := PMemoryBlocHeader((PAnsiChar(Self) - SizeOf(TMemoryBlocHeader)))^;
	try
		i := MaxNbSupportedVMTEntries - GIndex + 1;
		if i in [1..8] then
			FeedBackStr:= 'Call ' + TObjectVirtualMethodNames[i]
		else
			FeedBackStr:= 'Call ' + IntToStr(i) + '° virtual method';

    AlloAtStr := ' - had been created at ' + GetDebugInfoAt(Cardinal(T.CallerAddress[0])) + ')';
    DetroyAtStr := ' (destroyed at ' + GetDebugInfoAt(Cardinal(T.DestructionAdress));

		FeedBackStr:= FeedBackStr + ' on a FREED instance of ' + T.VMT.ClassName + DetroyAtStr + AlloAtStr;
		raise EBadInstance.Create(FeedBackStr) at Caller;
	except
		on EBadInstance do ;
	end;
	if ShowCallStack then
		for i := 1 to StoredCallStackDepth - 1 do
			if Integer(T.CallerAddress[i]) > 0 then
				try
					raise EStackUnwinding.Create('Unwinding level ' + chr(ord('0') + i))at T.CallerAddress[i]
				except
					on EStackUnwinding do ;
				end;
	ShowCallStack := False;
end;

//------------------------------------------------
function InterfaceErrorCaller: Pointer;
{Returns EBP + 16, which is OK only for InterfaceError !
It would be nice to make this routine local to InterfaceError, but I do not know hot to
implement it in this case - VM}
	asm
		cmp ebp, 0	//this can happen when there are no stack frames
		je @@EndOfStack
		mov	 eax,[EBP+16];
		sub	 eax, 5
		ret
		@@EndOfStack:
		mov eax, $FFFF
	end;

//------------------------------------------------
procedure ReleasedInstance.InterfaceError;
begin
	try
		OutputFileHeader := OutputFileHeader + #13#10'Exception: Calling an interface method on an freed Pascal instance @ ' + GetDebugInfoAt(Cardinal(InterfaceErrorCaller)) + #13#10;
		raise EInterfaceFreedInstance.Create('Calling an interface method on an freed Pascal instance')at InterfaceErrorCaller
	except
		on EInterfaceFreedInstance do
			;
	end;
end;

//------------------------------------------------
procedure ReleasedInstance.Error; //Don't change this, its a Hack! jm
asm
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);Inc(GIndex);
		JMP ReleasedInstance.RaiseExcept;
	 end;

//------------------------------------------------
function MemoryBlockDump(Block: PMemoryBlocHeader): string;
const
	MaxDump = 80;
var
	i,
		count: integer;
	s: string[MaxDump]; //ShortString
begin
	count := Block.AllocatedSize;

	if count > MaxDump then
		Count := MaxDump;

	Byte(s[0]) := count;
	move((PAnsiChar(Block) + SizeOf(TMemoryBlocHeader))^, s[1], Count);

	for i := 1 to Length(s) do
		if s[i] = #0 then s[i] := '.' else
			if s[i] < ' ' then
				s[i] := '?';

	Result := '  Dump: [' + String(s) + ']';
end;

//------------------------------------------------
procedure AddAllocatedObjectsClass(const C: TClass);
begin
	if NbClasses >= Length(AllocatedObjectsClasses) then
		begin
			UnMemChk;
			SetLength(AllocatedObjectsClasses, NbClasses * 2);
			MemChk;
		end;

	AllocatedObjectsClasses[NbClasses] := C;
	NbClasses := NbClasses + 1;
end;

//------------------------------------------------
procedure CollectNewInstanceOfClassForStats(const TheClass: TClass);
var	i: integer;
begin
	i := 0;
	while (i < AllocStatsCount) and (AllocatedObjectsClasses[i] <> TheClass) do
		i := i + 1;

	if i = AllocStatsCount then
		begin
			if AllocStatsCount = StatsArraysAllocatedPos then
				begin
					if StatsArraysAllocatedPos = 0 then
						StatsArraysAllocatedPos := 10;
					StatsArraysAllocatedPos := StatsArraysAllocatedPos * 2;
					UnMemChk;
					ReallocMem(AllocatedInstances, StatsArraysAllocatedPos * sizeof(Integer));
					MemChk;
				end;

			AddAllocatedObjectsClass(TheClass);
			AllocatedInstances[AllocStatsCount] := 1;
			AllocStatsCount := AllocStatsCount + 1;
		end
	else
		AllocatedInstances[i] := AllocatedInstances[i] + 1;
end;

var	LinkedListSynchro: TSynchroObject;
//------------------------------------------------
procedure AddBlockAtEndOfLinkedList(const B: PMemoryBlocHeader);
begin
	LinkedListSynchro.Acquire;
	PMemoryBlocHeader(B).PreceedingBlock:= LastBlock;
	PMemoryBlocHeader(B).NextBlock:= nil;
	if LastBlock <> nil then
		LastBlock.NextBlock:= B;
	LastBlock:= B;
	LinkedListSynchro.Release;
end;

//------------------------------------------------
procedure RemoveBlockFromLinkedList(const B: PMemoryBlocHeader);
begin
	LinkedListSynchro.Acquire;
	if B.NextBlock <> nil then
		PMemoryBlocHeader(B.NextBlock).PreceedingBlock:= B.PreceedingBlock;
	if B.PreceedingBlock <> nil then
		PMemoryBlocHeader(B.PreceedingBlock).NextBlock:= B.NextBlock;
	if LastBlock = B then
		LastBlock:= B.PreceedingBlock;
	LinkedListSynchro.Release;
end;


//old delphi AllocMemSize = all the memory used in an application
//it is not supported anymore. GetMemoryManagerState can be used to do the job
//Todo in the future
//-----------------------------------------------------------------------------------------
procedure StatisticMem(Size: NativeInt);
var AllocMemTotal : Integer;
    KpMaxUsg : boolean;
begin
  KpMaxUsg := KeepMaxMemoryUsage;
  AllocMemTotal := 0;
  if ComputeMemoryUsageStats then begin
    MemoryUsageStatsLoop := MemoryUsageStatsLoop + 1;
    if MemoryUsageStatsLoop = MemoryUsageStatsStep then
      MemoryUsageStatsLoop := 0;

    if (MemoryUsageStatsLoop = 0) or (Size > StatCollectionForce) then begin
      if MemoryUsageStatsCount = MemoryUsageStatsAllocatedPos then begin
        if MemoryUsageStatsAllocatedPos = 0 then
          MemoryUsageStatsAllocatedPos := 10;
        MemoryUsageStatsAllocatedPos := MemoryUsageStatsAllocatedPos * 2;
        UnMemChk;
        ReallocMem(MemoryUsageStats, MemoryUsageStatsAllocatedPos * sizeof(Integer));
        MemChk;
      end;

      MemoryUsageStats[MemoryUsageStatsCount] := AllocMemTotal;
      MemoryUsageStatsCount := MemoryUsageStatsCount + 1;
    end;
  end;

  if KpMaxUsg and (AllocMemTotal > MaxMemoryUsage) then
    MaxMemoryUsage := AllocMemTotal;
end;


//------------------------------------------------------------------------
procedure GetCallStacks(var Stack: TCallStack; FramesToSkip: Integer);
begin
  if StoredCallStackDepth <= 0 then exit;

  ZeroMemory(@Stack, SizeOf(Stack));
  RtlCaptureStackBackTrace(FramesToSkip, Length(Stack), @Stack, nil);
end;


//------------------------------------------------------------------------
procedure GetCallStackLst(var Stack: TCallStackLst; FramesToSkip: Integer);
begin
  ZeroMemory(@Stack, SizeOf(Stack));
  RtlCaptureStackBackTrace(FramesToSkip, Length(Stack), @Stack, nil);
end;


//-----------------------------------------------------------------------------
function LeakTrackingGetMem(Size: NativeInt): Pointer;
var MemPtr : PMemoryBlocHeader;
    SzPlusHdr, HdrSz, FtSz, SzPlusHdrFt: NativeInt;
    CalledInNewAnsiString, CalledInDynArraySetLength, CalledInNewInstance, CalledInNewUniString : boolean;
begin
  CalledInNewAnsiString := CallerIsNewAnsiString;
  CalledInDynArraySetLength := CallerIsDynamicArrayAllocation;
  CalledInNewUniString := CallerIsNewUniString;

	if CalledInNewAnsiString or CalledInNewUniString or CalledInDynArraySetLength or (not MemCheckActive) then begin		//No point to log memory allocations for reference counted strings.
 	  Result := OldMemoryManager.GetMem(Size);
		if Result = nil then
			raise OutOfMemory;
	end else begin
    HdrSz := SizeOf(TMemoryBlocHeader);
    FtSz := SizeOf(TMemoryBlockFooter);
    SzPlusHdr := Size + HdrSz;

    CalledInNewInstance := CallerIsNewInstance;
    if CalledInNewInstance then	begin
      MemPtr := OldMemoryManager.GetMem(SzPlusHdr);
      if MemPtr = nil then
        raise OutOfMemory;

      MemPtr^.KindOfBlock := MClass;
      GetCallStacks(MemPtr.CallerAddress, StackFrmToSkip);
      if StartingSum then
        CurMemUsage := CurMemUsage + SzPlusHdr;

    end	else begin	//Neither an object nor a string, this is a MUser
      SzPlusHdrFt := Size + HdrSz + FtSz;
      MemPtr := OldMemoryManager.GetMem(SzPlusHdrFt);
      if MemPtr = nil then
        raise OutOfMemory;
      MemPtr.KindOfBlock := MUser;
      GetCallStacks(MemPtr.CallerAddress, StackFrmToSkip);
      PMemoryBlockFooter(PAnsiChar(MemPtr) + SzPlusHdr )^ := EndOfBlock;
      if StartingSum then
        CurMemUsage := CurMemUsage + SzPlusHdrFt;
    end;

    AddBlockAtEndOfLinkedList(MemPtr);
    MemPtr.LastTimeStamp := TimeStampsCount - 1;
    MemPtr.DestructionAdress := nil;
    MemPtr.AllocatedSize := Size;
    MemPtr.MagicNumber := Magic;
    if IdentifyObjectFields then begin
      UnMemChk;
      CurrentlyAllocatedBlocksTree.Add(integer(MemPtr));
      MemChk;
    end;

    Inc(integer(MemPtr), HdrSz);

    StatisticMem(Size);
    result := MemPtr;
  end;

end;

//-----------------------------------------------------
function HeapCheckingGetMem(Size: NativeInt): Pointer;
begin
	HeapStatusSynchro.Acquire;
	try
		if HeapStatusesDifferent(LastHeapStatus, GetHeapStatus) then
			raise HeapCorrupted;
		Result := OldMemoryManager.GetMem(Size);
		UpdateLastHeapStatus;
	finally
		HeapStatusSynchro.Release;
	end;
end;

//-----------------------------------------------------
function MemoryBlockFreed(Block: PMemoryBlocHeader): Boolean;
begin
	Result := Block.DestructionAdress <> nil;
end;

//-----------------------------------------------------
function MemoryBlockOverwritten(Block: PMemoryBlocHeader): Boolean;
begin
	if (block.KindOfBlock = MClass) then
		Result:= false	//We don't put a footer for objects - This could be done if interesting
	else
		Result:= PMemoryBlockFooter(PAnsiChar(Block) + SizeOf(TMemoryBlocHeader) + Block.AllocatedSize)^ <> EndOfBlock;
end;

//-----------------------------------------------------
function MemCheckBlockCorrupted(Block: PMemoryBlocHeader): Boolean;
begin
	Result := MemoryBlockFreed(Block) or MemoryBlockOverwritten(Block);
end;


//Is the given block bad ?
//P is a block you may for example have created with GetMem, or P can be an object.
//Bad means you have written beyond the block's allocated space or the memory for this object was freed.
//If P was allocated before MemCheck was launched, we return False
//--------------------------------------------------------------------
function MemoryBlockCorrupted(P: Pointer): Boolean;
var	Block: PMemoryBlocHeader;
begin
	if PCardinal(PAnsiChar(P) - 4)^ = Magic then begin
			Block := PMemoryBlocHeader(PAnsiChar(P) - SizeOf(TMemoryBlocHeader));
			Result:= MemCheckBlockCorrupted(Block);
	end	else
		Result := False
end;

//copied and modified from System.Pas: replaces all INTERFACES in Pascal Objects with a reference to our dummy INTERFACE VMT
//------------------------------------------------------------------------------------------------------------------------------
procedure ReplaceInterfacesWithBadInterface(AClass: TClass; Instance: Pointer);
asm
				PUSH	EBX
				PUSH	ESI
				PUSH	EDI
				MOV	 EBX,EAX
				MOV	 EAX,EDX
				MOV	 EDX,ESP
		@@0:	MOV	 ECX,[EBX].vmtIntfTable
				TEST	ECX,ECX
				JE	  @@1
				PUSH	ECX
		@@1:	MOV	 EBX,[EBX].vmtParent
				TEST	EBX,EBX
				JE	  @@2
				MOV	 EBX,[EBX]
				JMP	 @@0
		@@2:	CMP	 ESP,EDX
				JE	  @@5
		@@3:	POP	 EBX
				MOV	 ECX,[EBX].TInterfaceTable.EntryCount
				ADD	 EBX,4
		@@4:	LEA	 ESI, BadInterfaceVMT // mettre dans ESI l'adresse du début de MyInterfaceVMT: correct ?????
				MOV	 EDI,[EBX].TInterfaceEntry.IOffset
				MOV	 [EAX+EDI],ESI
				ADD	 EBX,TYPE TInterfaceEntry
				DEC	 ECX
				JNE	 @@4
				CMP	 ESP,EDX
				JNE	 @@3
		@@5:	POP	 EDI
				POP	 ESI
				POP	 EBX
end;

//-----------------------------------------------------------------------------------------------------------------
function FindMem(Base, ToFind: pointer; Nb: integer): integer; // Base = instance, Nb = nombre de bloc (HORS VMT!)
asm
			// eax=base; edx=Tofind; ecx=Nb
			@loop:
			cmp [eax+ecx*4], edx
			je @found
			dec ecx
			jne  @loop

			@found:
			mov eax,ecx
	 end;

procedure AddFieldInfo(const FieldAddress: Pointer; const OwnerClass: TClass; const FieldPos: integer);
begin
	UnMemChk;

	if NotDestroyedFieldsCount = NotDestroyedFieldsAllocatedSpace then
		begin
			if NotDestroyedFieldsAllocatedSpace = 0 then
				NotDestroyedFieldsAllocatedSpace := 10;
			NotDestroyedFieldsAllocatedSpace := NotDestroyedFieldsAllocatedSpace * 2;
			ReallocMem(NotDestroyedFields, NotDestroyedFieldsAllocatedSpace * sizeof(integer));
			ReallocMem(NotDestroyedFieldsInfos, NotDestroyedFieldsAllocatedSpace * sizeof(integer));
		end;

	NotDestroyedFields[NotDestroyedFieldsCount] := integer(FieldAddress);
	NotDestroyedFieldsInfos[NotDestroyedFieldsCount] := TFieldInfo.Create(OwnerClass, FieldPos);
	NotDestroyedFieldsCount := NotDestroyedFieldsCount + 1;

	MemChk;
end;

//--------------------------------------------------------------------------------------
function LeakTrackingFreeMem(P: Pointer): Integer;
var Block: PMemoryBlocHeader;
	I: integer;
  HdrSz, FtSz: NativeInt;
  Msg : String;
begin
	if PCardinal(PAnsiChar(P) - 4)^ = Magic then begin  // It is a block marked with Magic
			Block := PMemoryBlocHeader(PAnsiChar(P) - SizeOf(TMemoryBlocHeader));
			if CollectStatsAboutObjectAllocation and (Block.KindOfBlock = MClass) then
				CollectNewInstanceOfClassForStats(TObject(P).ClassType);

			if IdentifyObjectFields then begin //not tested
        if (Block.KindOfBlock = MClass) and (TObject(P).InheritsFrom(IdentifyFieldsOfObjectsConformantTo)) then
          for i := 1 to (Block.AllocatedSize div 4) - 1 do
            if (PInteger(PAnsiChar(P) + i * 4)^ > SizeOf(TMemoryBlocHeader)) and CurrentlyAllocatedBlocksTree.Has(PInteger(PAnsiChar(P) + i * 4)^ - SizeOf(TMemoryBlocHeader)) then
              AddFieldInfo(Pointer(PInteger(PAnsiChar(P) + i * 4)^ - SizeOf(TMemoryBlocHeader)), TObject(P).ClassType, i);

        UnMemChk;
        if not MemoryBlockFreed(Block) then
          begin
            Assert(CurrentlyAllocatedBlocksTree.Has(integer(Block)), 'freemem: block not among allocated ones');
            CurrentlyAllocatedBlocksTree.Remove(integer(Block));
          end;
        MemChk;
      end;

			if MemoryBlockFreed(Block) then	begin
        GetCallStackLst(TmpCallers, 3);      //J Lin 28 July 2023
        For I := 0 to TmpCallStackDepth - 2 do begin
          Msg := 'Exception: second release of block attempt, at ' + GetDebugInfoAt(Cardinal(TmpCallers[I])) + #13#10;
          try
            OutputFileHeader := OutputFileHeader + Msg + ''#13#10;
            raise EMemoryLeak.Create(Msg) at TmpCallers[I];
          except
            on EMemoryLeak do
              Msg := '';
          end;
        end;
			end	else begin
        if MemoryBlockOverwritten(Block) then	begin
          try
            OutputFileHeader := OutputFileHeader + #13#10'Exception: memory damaged beyond block allocated space, allocated at ' + GetDebugInfoAt(Cardinal(BlockAllocationAddress(P))) + #13#10;
            raise EMemoryLeak.Create('memory damaged beyond block allocated space, allocated at ' + GetDebugInfoAt(Cardinal(BlockAllocationAddress(P)))) at CallerOfCaller;
          except
            on EMemoryLeak do ;
          end;
        end;


        if StartingSum then begin
          HdrSz := SizeOf(TMemoryBlocHeader);
          FtSz := SizeOf(TMemoryBlockFooter);
          SumMemFreed := SumMemFreed + Block.AllocatedSize + HdrSz + FtSz;
          CurMemUsage := CurMemUsage - (Block.AllocatedSize + HdrSz + FtSz);
        end;

        if (Block.AllocatedSize > BlockSzLmt) or
           (DeallocateSpecifiedClass and (Block.KindOfBlock = MClass) and (TObject(P) is DeallocationClass)) then	begin
          Block.DestructionAdress := Pointer(FreedMagic);  //Mark it as Freed. J Lin 28 July 2023
          RemoveBlockFromLinkedList(Block);
          OldMemoryManager.FreeMem(Block);
        end	else begin	//Normal case, not an error
          if Block.KindOfBlock = MClass then
            Block.DestructionAdress:= CallerOfFreeInstance
          else
            Block.DestructionAdress:= CallerOf_FreeMem;

          if WipeOutMemoryOnFreeMem then
            if Block.KindOfBlock = MClass then begin
                Block.VMT := TObject(P).ClassType;
                FillChar((PAnsiChar(P) + 4)^, Block.AllocatedSize - 4, CharToUseToWipeOut);
                PInteger(P)^ := Integer(FreedInstance);
                if DanglingInterfacesVerified then
                  ReplaceInterfacesWithBadInterface(Block.VMT, TObject(P))
            end else
              FillChar(P^, Block.AllocatedSize, CharToUseToWipeOut);
        end;
			end;

			Result := 0;
		end
	else
		Result := OldMemoryManager.FreeMem(P);
end;

//---------------------------------------------------
function HeapCheckingFreeMem(P: Pointer): Integer;
begin
	if HeapStatusesDifferent(LastHeapStatus, GetHeapStatus) then
		raise HeapCorrupted;

	Result := OldMemoryManager.FreeMem(P);

	UpdateLastHeapStatus;
end;



//---------------------------------------------------
function InvalidAllocMem(ASize: NativeInt): Pointer;
begin
  Result := nil;
end;

//---------------------------------------------------
function InvalidRegisterAndUnRegisterMemoryLeak(APointer: Pointer): Boolean;
begin
  Result := False;
end;


//---------------------------------------------------------------------
function LeakTrackingReallocMem(P: Pointer; Size: NativeInt): Pointer;
var Block: PMemoryBlocHeader;
begin
	if PCardinal(PAnsiChar(P) - 4)^ = Magic then begin
    GetMem(Result, Size);
    Block:= PMemoryBlocHeader(PAnsiChar(Result) - SizeOf(TMemoryBlocHeader));
    GetCallStacks(Block.CallerAddress, 0);
    Block.CallerAddress[0]:= CallerOfReallocMem;
    Block.KindOfBlock := MReallocedUser;
    if Size > PMemoryBlocHeader(PAnsiChar(P) - SizeOf(TMemoryBlocHeader)).AllocatedSize then
      Move(P^, Result^, PMemoryBlocHeader(PAnsiChar(P) - SizeOf(TMemoryBlocHeader)).AllocatedSize)
    else
      Move(P^, Result^, Size);
    LeakTrackingFreeMem(P);
  end	else
		Result := OldMemoryManager.ReallocMem(P, Size);
end;

//---------------------------------------------------------------------
function HeapCheckingReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
	if HeapStatusesDifferent(LastHeapStatus, GetHeapStatus) then
		raise HeapCorrupted;

	Result := OldMemoryManager.ReallocMem(P, Size);

	UpdateLastHeapStatus;
end;

//---------------------------------------------------------------------
procedure UnMemChk;
begin
	SetMemoryManager(OldMemoryManager);
	MemCheckActive := False;
end;

//is the memory at P made of C on N bytes ?
//---------------------------------------------------------------------
function IsMemFilledWithChar(P: Pointer; N: Integer; C: AnsiChar): boolean;
asm
	//EAX: P - EDX: N - CL: C
	@loop:
	cmp   [eax+edx-1],cl
	jne   @diff
	dec   edx
	jne   @loop
	mov   eax,1
	ret
	@diff:
	xor   eax,eax
end;

//--------------------------------------------------------------------
procedure ShowCallerFuncPos(uwLevel : integer; CallerAdd : Pointer);
var	emLeakException : Exception;
begin
  try
    emLeakException := EStackUnwinding.Create('Unwinding level ' + IntToStr( uwLevel));
    raise emLeakException at CallerAdd;
  except

  end;
end;

//traverses the allocated blocks list and for each one, raises exceptions showing the memory leaks
//-------------------------------------------------------------------------------------------------
procedure GoThroughAllocatedBlocks;
var
	Block: PMemoryBlocHeader;
	i: integer;
	S: String;
  msg, AlloAtStr, DetroyAtStr : String;
  isMemAllChar : Boolean;
  emLeakException : Exception;
  Hdl : THandle;
  ShowStackLst : TShowStackLst;
  suc : Boolean;
begin
  Hdl := LoadLibrary('MemLeakRpt.dll');
  if RaiseExceptionsOnEnd then begin
		UnMemChk;
		Block := LastBlock;	//note: no thread safety issue here
		while Block <> nil do begin
      if BlocksToShow[Block.KindOfBlock] then begin
        if not MemoryBlockFreed(Block) then begin //this is a leak
          case Block.KindOfBlock of
            MClass: S := TObject(PAnsiChar(Block) + SizeOf(TMemoryBlocHeader)).ClassName;
            MUser:  S := 'User';
            MReallocedUser: S := 'Realloc';
          end;

          if Hdl <> 0 then begin
            ShowStackLst := GetProcAddress(Hdl, 'ShowCallerList');
            if @ShowStackLst <> nil then begin
              Suc := ShowStackLst(Block.CallerAddress, MapFileName, StoredCallStackDepth, DisplOfExe, @ShowCallerFuncPos);
              if not suc then
                break;
            end;
          end else begin
            if (BlocksToShow[Block.KindOfBlock]) and ((Block.KindOfBlock <> MClass) or (TObject(PAnsiChar(Block) + SizeOf(TMemoryBlocHeader)) is InstancesConformingToForReporting)) then begin
              msg := S + ' allocated at ' + GetDebugInfoAt(Cardinal(Block.CallerAddress[0]));
              try
                emLeakException := EMemoryLeak.Create(msg);
                raise emLeakException at Block.CallerAddress[0];
              except
                on EMemoryLeak do
                ;
              end;
            end;

            for I := 1 to StoredCallStackDepth - 1 do begin
              if Integer(Block.CallerAddress[i]) > 0 then
                try
                  emLeakException := EStackUnwinding.Create(S + ' unwinding level ' + chr(ord('0') + i));
                  raise emLeakException at Block.CallerAddress[i]
                except
                  on EStackUnwinding do ;
                end;
            end;
          end;
        end else begin	 //Block.DestructionAdress = Nil  //this is not a leak
          isMemAllChar := IsMemFilledWithChar(PAnsiChar(Block) + SizeOf(TMemoryBlocHeader) + 4, Block.AllocatedSize - 5, CharToUseToWipeOut);
          if CheckWipedBlocksOnTermination and (Block.AllocatedSize > 5) and (Block.AllocatedSize <= DoNotCheckWipedBlocksBiggerThan) and (not isMemAllChar) then begin
            try
              AlloAtStr := '- Block allocated at ' + GetDebugInfoAt(Cardinal(Block.CallerAddress[0]));
              DetroyAtStr := ' - destroyed at ' + GetDebugInfoAt(Cardinal(Block.DestructionAdress));
              raise EFreedBlockDamaged.Create('Destroyed block damaged ' + AlloAtStr + DetroyAtStr) at Block.CallerAddress[0]
            except
              on EFreedBlockDamaged do ;
            end;
          end;
        end;
      end;

      Block := Block.PreceedingBlock;
    end;
	end;

end;

// The original finalization order is : .... MemCheck, classes, variants, sysUtils, System.Character, Winapi.PsAPI, Winapi.Windows; systems
//                                               41      36        19         17         11                 9               4        1
// It will cause bug :
//1)MemCheck reset MemoryManager back to it's original in finalization, thus
//  in System.class, destructor TThread.Destroy calls DoneThreadSynchronization -> ThreadLock.DisposeOf, which calls system's orinal memory manager's FreeMem
//  As ThreadLock is created by MemCheck, it should be freed by MemCheck.

// 2)class destructor TEncoding.Destroy is called after unit System.SysUtils:finalization,
//   if MemCheck is finalized earlier then here, same bug(Allocate deallocate not match) will happen
//
//Changes the order in which MemCheck finalizations will occur just before System.Character
//----------------------------------------------------------------------------------------------------------------------------------------------------------
procedure ChangeFinalizationsOrder;
type	PPackageUnitEntry = ^PackageUnitEntry;
var
	I, J, memChkI: integer;
	CurrentUnitInfo: PackageUnitEntry;

	ProcessHandle: THandle;
	BytesWritten: NativeUInt;
  UnitPtr : Pointer;
begin
  asm
    push eax;
    mov eax, offset System@Character;//Winapi@Windows; Winapi.PsAPI
    mov UnitPtr, eax;
    pop eax;
  end;

  SetLength(UnitTbls, UnitsInfo.UnitCount + 2);
  J := 0;   memChkI := 0;
	for I:= 0 to UnitsInfo.UnitCount - 1 do	begin
		CurrentUnitInfo:= UnitsInfo.UnitInfo^[i];
    UnitTbls[J] := CurrentUnitInfo;
    J := J + 1;

    if (CurrentUnitInfo.Init = UnitPtr) then begin
      memChkI := J;                                     //memchkPtr will insert here at memChkI
      J := J + 1;                                       //Make a room for MemchkPtr
      break;
    end;
  end;

	for I:= J to UnitsInfo.UnitCount - 1 do	begin
		CurrentUnitInfo:= UnitsInfo.UnitInfo^[i];
    if (CurrentUnitInfo.Init = memchkPtr) then          //Found the MemCheck initialization address
      UnitTbls[memChkI] := CurrentUnitInfo
    else begin
      UnitTbls[J] := CurrentUnitInfo;
      J := J + 1;
    end;
  end;

  ProcessHandle:= openprocess(PROCESS_ALL_ACCESS, True, GetCurrentProcessId);
	for I:= 0 to UnitsInfo.UnitCount - 1 do  //FInit : Unit Finalization address
    WriteProcessMemory(ProcessHandle, @UnitsInfo.UnitInfo^[i].FInit, @UnitTbls[I].FInit, 4, BytesWritten);
  CloseHandle(ProcessHandle);

  UnitTbls := Nil;
end;

//---------------------------------------------------------------------------
function UnitWhichContainsAddress(const Address: Cardinal): TUnitDebugInfos;
var
	Start, Finish, Pivot: integer;
begin
	Start := 0;
	Finish := UnitsCount - 1;
	Result := nil;

	while Start <= Finish do
		begin
			Pivot := Start + (Finish - Start) div 2;

			if TUnitDebugInfos(Units[Pivot]).Addresses[0].Address > Address then
				Finish := Pivot - 1
			else
				if TUnitDebugInfos(Units[Pivot]).Addresses[Length(TUnitDebugInfos(Units[Pivot]).Addresses) - 1].Address < Address then
					Start := Pivot + 1
				else
					begin
						Result := Units[Pivot];
						Start := Finish + 1;
					end;
		end;
end;

function RoutineWhichContainsAddress(const Address: Cardinal): string;
var
	Start, Finish, Pivot: integer;
begin
	Start := 0;
	Result := NoDebugInfo;
	Finish := RoutinesCount - 1;

	while Start <= Finish do
		begin
			Pivot := Start + (Finish - Start) div 2;

			if TRoutineDebugInfos(Routines[Pivot]).StartAddress > Address then
				Finish := Pivot - 1
			else
				if TRoutineDebugInfos(Routines[Pivot]).EndAddress < Address then
					Start := Pivot + 1
				else
					begin
						Result := ' Routine ' + String(TRoutineDebugInfos(Routines[Pivot]).Name);
						Start := Finish + 1;
					end;
		end;
end;

type
	TExceptionProc = procedure(Exc: TObject; Addr: Pointer);

var
	InitialExceptionProc: TExceptionProc;
	VersionInfo: string;
  UseMemChkException : boolean = False;

procedure MyExceptProc(Exc: TObject; Addr: Pointer);
var S: TCallStack;
begin
	Writeln(SevereExceptionsLogFile, '');
	Writeln(SevereExceptionsLogFile, '********* Severe exception detected - ' + DateTimeToStr(Now) + ' *********');
	Writeln(SevereExceptionsLogFile, VersionInfo);
	Writeln(SevereExceptionsLogFile, 'Exception code: ' + Exc.ClassName);
	Writeln(SevereExceptionsLogFile, 'Exception address: ' + GetDebugInfoAt(Cardinal(Addr)));
	Writeln(SevereExceptionsLogFile, #13#10'Call stack (oldest call at bottom):');
	GetCallStacks(S, StackFrmToSkip);
	Writeln(SevereExceptionsLogFile, CallStackTextualRepresentation(S, ''));
	Writeln(SevereExceptionsLogFile, '*****************************************************************');
	Writeln(SevereExceptionsLogFile, '');
	InitialExceptionProc(Exc, Addr);
	//The closing of the file is done in the finalization
end;

//-----------------------------------------------------------------
procedure LogSevereExceptions(const WithVersionInfo: string);
const	FileNameBufSize = 1000;
var	LogFileName: string;
begin
	if ExceptProc <> @MyExceptProc then	begin //	not installed yet ?
    try
      SetLength(LogFileName, FileNameBufSize);
      GetModuleFileName(0, PChar(LogFileName), FileNameBufSize);
      LogFileName := copy(LogFileName, 1, pos('.', LogFileName)) + 'log';

      AssignFile(SevereExceptionsLogFile, LogFileName);

      if FileExists(LogFileName) then
        Append(SevereExceptionsLogFile)
      else
        Rewrite(SevereExceptionsLogFile);
    except
    end;

    InitialExceptionProc := ExceptProc;
    ExceptProc := @MyExceptProc;
    VersionInfo := WithVersionInfo;
    UseMemChkException := True;
  end;
end;

//------------------------------------------
function IsMemCheckActive: boolean;
begin
	Result := MemCheckActive
end;

//------------------------------------------
constructor TUnitDebugInfos.Create(const AName: string; const NbLines: Cardinal);
begin
	Name := AName;

	SetLength(Addresses, NbLines);
end;

//------------------------------------------
constructor TRoutineDebugInfos.Create(const AName: AnsiString; const AStartAddress: Cardinal; const ALength: Cardinal);
begin
	Name := AName;
	StartAddress := AStartAddress;
	EndAddress := StartAddress + ALength - 1;
end;

constructor TAddressToLine.Create(const AAddress, ALine: Cardinal);
begin
	Address := AAddress;
	Line := ALine
end;

function TUnitDebugInfos.LineWhichContainsAddress(const Address: Cardinal): string;
var
	Start, Finish, Pivot: Cardinal;
begin
	if Addresses[0].Address > Address then
		Result := ''
	else
		begin
			Start := 0;
			Finish := Length(Addresses) - 1;

			while Start < Finish - 1 do
				begin
					Pivot := Start + (Finish - Start) div 2;

					if Addresses[Pivot].Address = Address then
						begin
							Start := Pivot;
							Finish := Start
						end
					else
						if Addresses[Pivot].Address > Address then
							Finish := Pivot
						else
							Start := Pivot
				end;

			Result := ' Line ' + IntToStr(Addresses[Start].Line);
		end;
end;

type
	SRCMODHDR = packed record
		_cFile: Word;
		_cSeg: Word;
		_baseSrcFile: array[0..MaxListSize] of Integer;
	end;

	SRCFILE = packed record
		_cSeg: Word;
		_nName: Integer;
		_baseSrcLn: array[0..MaxListSize] of Integer;
	end;

	SRCLN = packed record
		_Seg: Word;
		_cPair: Word;
		_Offset: array[0..MaxListSize] of Integer;
	end;

	PSRCMODHDR = ^SRCMODHDR;
	PSRCFILE = ^SRCFILE;
	PSRCLN = ^SRCLN;

	TArrayOfByte = array[0..MaxListSize] of Byte;
	TArrayOfWord = array[0..MaxListSize] of Word;
	PArrayOfByte = ^TArrayOfByte;
	PArrayOfWord = ^TArrayOfWord;
	PArrayOfPointer = ^TArrayOfPointer;
	TArrayOfPointer = array[0..MaxListSize] of PArrayOfByte;

procedure AddRoutine(const Name: AnsiString; const Start, Len: Cardinal);
begin
	if Length(Routines) <= RoutinesCount then
		SetLength(Routines, Max(RoutinesCount * 2, 1000));

	Routines[RoutinesCount] := TRoutineDebugInfos.Create(Name, Start, Len);
	RoutinesCount := RoutinesCount + 1;
end;

procedure AddUnit(const U: TUnitDebugInfos);
begin
	if Length(Units) <= UnitsCount then
		SetLength(Units, Max(UnitsCount * 2, 1000));

	Units[UnitsCount] := U;
	UnitsCount := UnitsCount + 1;
end;

procedure dumpsymbols(NameTbl: PArrayOfPointer; sstptr: PArrayOfByte; size: integer);
//Copyright (C) Tenth Planet Software Intl., Clive Turvey 1998. All rights reserved. - Reused & modified by SG with permission
var
	len, sym: integer;
  NmStr : AnsiString;
begin
	while size > 0 do
		begin
			len := PWord(@sstptr^[0])^;
			sym := PWord(@sstptr^[2])^;

			INC(len, 2);

			if ((sym = $205) or (sym = $204)) and (PInteger(@sstptr^[40])^ > 0) then begin
        NmStr := PAnsiChar(NameTbl^[PInteger(@sstptr^[40])^ - 1]);
				AddRoutine(NmStr, PInteger(@sstptr^[28])^, PInteger(@sstptr^[16])^);
      end;

			if (len = 2) then
				size := 0
			else
				begin
					sstptr := PArrayOfByte(@sstptr^[len]);
					DEC(size, len);
				end;
		end;
end;

//------------------------------------------------------------------------------------
procedure dumplines(NameTbl: PArrayOfPointer; sstptr: PArrayOfByte; size: integer);
var	srcmodhdr: PSRCMODHDR;
	i: Word;
	srcfile: PSRCFILE;
	srcln: PSRCLN;
	k: Word;
	CurrentUnit: TUnitDebugInfos;
  FileNm : String;
begin
	if size > 0 then begin
    srcmodhdr := PSRCMODHDR(sstptr);
    for i := 0 to pred(srcmodhdr^._cFile) do begin
      srcfile := PSRCFILE(@sstptr^[srcmodhdr^._baseSrcFile[i]]);
      if srcfile^._nName > 0 then begin//note: assume that the code is always in segment #1. If this is not the case, Houston !  - VM
        srcln := PSRCLN(@sstptr^[srcfile^._baseSrcLn[0]]);

        FileNm := String(PAnsiChar(NameTbl^[srcfile^._nName - 1]));
        CurrentUnit := TUnitDebugInfos.Create(ExtractFileName(FileNm), srcln^._cPair);
        AddUnit(CurrentUnit);

        for k := 0 to pred(srcln^._cPair) do
          CurrentUnit.Addresses[k] := TAddressToLine.Create(Integer(PArrayOfPointer(@srcln^._Offset[0])^[k]), Integer(PArrayOfWord(@srcln^._Offset[srcln^._cPair])^[k]));
      end;
    end;
  end;
end;

//Copyright (C) Tenth Planet Software Intl., Clive Turvey 1998. All rights reserved. - Reused & modified by SG with permission
//-------------------------------------------------------------------------------------------------------------------------------
procedure GetProjectInfos;
var
	AHeader: packed record
		Signature: array[0..3] of AnsiChar;
		AnInteger: Integer;
	end;
	k: integer;
	j: Word;
	lfodir: Integer;
	SstFrameSize: integer;
	SstFrameElem: PArrayOfByte;
	ssttype, sstsize, sstbase: Integer;
	x, y, z: Integer;
	sstbuf: PArrayOfByte;
	OldFileMode: integer;
	AFileOfByte: file of Byte;
	Names: PArrayOfByte;
	NameTbl: PArrayOfPointer;
	SstFrame: PArrayOfByte;
	ifabase: Integer;
	cdir, cbdirentry: word;
	FileName: string;
begin
	RoutinesCount := 0;
	UnitsCount := 0;
	OldFileMode := FileMode;
	FileMode := 0;
	SetLength(FileName, MAX_PATH + 1);
	SetLength(FileName, GetModuleFileName(HInstance, PChar(FileName), MAX_PATH));
	AssignFile(AFileOfByte, FileName);
	Reset(AFileOfByte);
	Names := nil;
	NameTbl := nil;
	Seek(AFileOfByte, FileSize(AFileOfByte) - SizeOf(AHeader));
	BlockRead(AFileOfByte, AHeader, SizeOf(AHeader));
	if (AHeader.Signature = 'FB09') or (AHeader.Signature = 'FB0A') then begin
    ifabase := FilePos(AFileOfByte) - AHeader.AnInteger;
    Seek(AFileOfByte, ifabase);
    BlockRead(AFileOfByte, AHeader, SizeOf(AHeader));
    if (AHeader.Signature = 'FB09') or (AHeader.Signature = 'FB0A') then begin
      lfodir := ifabase + AHeader.AnInteger;
      if lfodir >= ifabase then begin
        Seek(AFileOfByte, lfodir);
        BlockRead(AFileOfByte, j, SizeOf(Word));
        BlockRead(AFileOfByte, cbdirentry, SizeOf(Word));
        BlockRead(AFileOfByte, cdir, SizeOf(Word));
        Seek(AFileOfByte, lfodir + j);
        SstFrameSize := cdir * cbdirentry;
        getmem(SstFrame, SstFrameSize);
        BlockRead(AFileOfByte, SstFrame^, SstFrameSize);
        for k := 0 to pred(cdir) do begin
          SstFrameElem := PArrayOfByte(@SstFrame^[k * cbdirentry]);
          ssttype := PWord(@SstFrameElem^[0])^;
          if (ssttype = $0130) then begin
            sstbase := ifabase + PInteger(@SstFrameElem^[4])^;
            sstsize := PInteger(@SstFrameElem^[8])^;
            getmem(Names, sstsize);
            Seek(AFileOfByte, sstbase);
            BlockRead(AFileOfByte, Names^, sstsize);
            y := PInteger(@Names^[0])^;
            getmem(NameTbl, sizeof(Pointer) * y);
            z := 4;
            for x := 0 to pred(y) do begin
              NameTbl^[x] := PArrayOfByte(@Names^[z + 1]);
              z := z + Names^[z] + 2;
            end;
          end;
        end;

        for k := 0 to pred(cdir) do begin
          SstFrameElem := PArrayOfByte(@SstFrame^[k * cbdirentry]);
          ssttype := PWord(@SstFrameElem^[0])^;
          sstbase := ifabase + PInteger(@SstFrameElem^[4])^;
          sstsize := PInteger(@SstFrameElem^[8])^;
          getmem(sstbuf, sstsize);
          Seek(AFileOfByte, sstbase);
          BlockRead(AFileOfByte, sstbuf^, sstsize);
          if (ssttype = $0125) then
            dumpsymbols(NameTbl, PArrayOfByte(@sstbuf^[4]), sstsize - 4);
          if (ssttype = $0127) then
            dumplines(NameTbl, sstbuf, sstsize);
          FreeMem(sstbuf);
        end;

        FreeMem(Names);
        FreeMem(NameTbl);
        FreeMem(SstFrame);
      end;
    end;
  end;

	CloseFile(AFileOfByte);
	FileMode := OldFileMode;
end;

//-------------------------------
procedure BadDestroy;
begin
	Writeln('bad destroy');
end;


//-------------------------------
procedure InitializeOnce;
var	i: integer;
begin
	if not MemCheckInitialized then		//once mechanism
		begin
			SetDispl;
			OutOfMemory := EOutOfMemory.Create('Memcheck is not able to allocate memory, due to system resource lack');
			HeapCorrupted := Exception.Create('Heap corrupted');
			ChangeFinalizationsOrder;
			MemCheckInitialized := True;
			GIndex := 0;
			LastBlock := nil;

			for I := 0 to MaxNbSupportedVMTEntries do	begin
				BadObjectVMT.B[I] := PAnsiChar(@ReleasedInstance.Error) + 6 * I;
				BadInterfaceVMT[I] := PAnsiChar(@ReleasedInstance.InterfaceError);
			end;

			FreedInstance := PAnsiChar(ReleasedInstance) + vmtMethodTable;
			Move(FreedInstance^, BadObjectVMT.A, 20);
			FreedInstance := PAnsiChar(@BadObjectVMT.B[8]);

			if IdentifyObjectFields then
				CurrentlyAllocatedBlocksTree := TIntegerBinaryTree.Create;
			if CollectStatsAboutObjectAllocation then
				SetLength(AllocatedObjectsClasses, 100);

			GetProjectInfos;

			GetMemoryManager(OldMemoryManager);

			LinkedListSynchro:= TCriticalSection.Create;

			if CheckHeapStatus then
				HeapStatusSynchro:= TSynchroObject.Create;
		end;
end;

//---------------------------------------------------------------
function CallStacksEqual(const CS1, CS2: TCallStack): Boolean;
var
	i: integer;
begin
	Result := True;
	i := 0;
	while (Result) and (i < StoredCallStackDepth - 1) do
		begin
			Result := Result and (CS1[i] = CS2[i]);
			i := i + 1;
		end;
end;

type
	TLeak = class
	public
		fID: integer;

		fBlock: PMemoryBlocHeader;
		fOccurences: integer;

		fWasFieldOfAnotherObject: Boolean;
		fOwnerClassName: string;
		fOtherFieldIndex: integer;
		fOtherIsDestroyed: Boolean;

		constructor Create(ABlock: PMemoryBlocHeader);
		function IsEqual(const Other: TLeak): Boolean;
		procedure AddOccurence;
		property Occurences: integer read fOccurences;
		property Block: PMemoryBlocHeader read fBlock;
		property WasFieldOfAnotherObject: Boolean read fWasFieldOfAnotherObject;
		property OtherObjectClassName: string read fOwnerClassName;
		property OtherFieldIndex: integer read fOtherFieldIndex;
		property OtherIsDestroyed: Boolean read fOtherIsDestroyed;
		procedure OutputToFile(const F: Text);
		procedure OutputOneLineToFile(const F: Text);
	end;

	TLeakList = class
	public
		fItems: array of TLeak;
		fCapacity: integer;
		fCount: integer;

		procedure Add(const L: TLeak);
		constructor Create;
		function Item(const I: integer): TLeak;
		property Count: integer read fCount;
	end;

	TBlockList = class
	public
		fItems: array of PMemoryBlocHeader;
		fCapacity: integer;
		fCount: integer;

		procedure Add(const B: PMemoryBlocHeader);
		constructor Create;
		function Item(const I: integer): PMemoryBlocHeader;
		property Count: integer read fCount;
	end;

constructor TLeak.Create(ABlock: PMemoryBlocHeader);
begin
	inherited Create;

	fBlock := ABlock;
	fOccurences := 1;
end;

procedure TLeak.OutputToFile(const F: Text);
begin
	Write(F, 'Leak #', fID, ' ');

	case Block.KindOfBlock of
		MClass:
			WriteLn(F, 'Instance of ', TObject(PAnsiChar(Block) + SizeOf(TMemoryBlocHeader)).ClassName);
		MUser:
			WriteLn(F, 'User allocated memory (GetMem)');
		MReallocedUser:
			WriteLn(F, 'Reallocated memory (ReallocMem)');
	end;

	WriteLn(F, #9'Size: ', Block.AllocatedSize);
	if fOccurences > 1 then
		WriteLn(F, #9, fOccurences, ' Occurences')
	else
		WriteLn(F, #9, fOccurences, ' Occurence');

	if fWasFieldOfAnotherObject then
		begin
			Write(F, #9'Was field #', fOtherFieldIndex, ' of an instance of ', fOwnerClassName);

			if fOtherIsDestroyed then
				WriteLn(F, ' (destroyed)')
			else
				WriteLn(F, ' (not destroyed)');
		end;

	WriteLn(F, CallStackTextualRepresentation(Block.CallerAddress, #9));
end;

procedure TLeak.OutputOneLineToFile(const F: Text);
begin
	case Block.KindOfBlock of
		MClass:
			Write(F, '* Instance of ', TObject(PAnsiChar(Block) + SizeOf(TMemoryBlocHeader)).ClassName);
		MUser:
			Write(F, '* User allocated memory (GetMem)');
		MReallocedUser:
			Write(F, '* Reallocated memory (ReallocMem)');
	end;

	Write(F, ' (Leak #', fID, ') ');

	WriteLn(F, 'Size: ', Block.AllocatedSize);
end;

function TLeak.IsEqual(const Other: TLeak): Boolean;
begin
	Result := (fBlock.KindOfBlock = Other.Block.KindOfBlock) and (fBlock.AllocatedSize = Other.Block.AllocatedSize);

	if fBlock.KindOfBlock = MClass then
		Result := Result and (TObject(PAnsiChar(fBlock) + SizeOf(TMemoryBlocHeader)).ClassName = TObject(PAnsiChar(Other.Block) + SizeOf(TMemoryBlocHeader)).ClassName);

	Result := Result and (WasFieldOfAnotherObject = Other.WasFieldOfAnotherObject);

	if WasFieldOfAnotherObject then
		Result := Result and (OtherObjectClassName = Other.OtherObjectClassName) and (OtherFieldIndex = Other.OtherFieldIndex) and (OtherIsDestroyed = Other.OtherIsDestroyed);

	Result := Result and CallStacksEqual(fBlock.CallerAddress, Other.Block.CallerAddress)
end;

procedure TLeak.AddOccurence;
begin
	fOccurences := fOccurences + 1
end;

procedure TLeakList.Add(const L: TLeak);
begin
	if Count = fCapacity then
		begin
			fCapacity := fCapacity * 2;
			SetLength(fItems, fCapacity);
		end;

	fItems[fCount] := L;
	fCount := fCount + 1;
end;

constructor TLeakList.Create;
begin
	inherited Create;

	fCapacity := 10;
	fCount := 0;
	SetLength(fItems, fCapacity);
end;

function TLeakList.Item(const I: integer): TLeak;
begin
	Assert((i >= 0) and (i < fCount), 'TLeakList.Item: out of bounds');

	Result := fItems[i]
end;

procedure TBlockList.Add(const B: PMemoryBlocHeader);
begin
	if Count = fCapacity then
		begin
			fCapacity := fCapacity * 2;
			SetLength(fItems, fCapacity);
		end;

	fItems[fCount] := B;
	fCount := fCount + 1;
end;

constructor TBlockList.Create;
begin
	inherited Create;

	fCapacity := 10;
	fCount := 0;
	SetLength(fItems, fCapacity);
end;

function TBlockList.Item(const I: integer): PMemoryBlocHeader;
begin
	Assert((i >= 0) and (i < fCount), 'TBlockList.Item: out of bounds');

	Result := fItems[i]
end;

procedure GetLeaks(const LeaksList, ChronogicalInfo: TLeakList; const MaxNumberOfLeaks: integer; var StoppedDueToMaxLeak: Boolean);
var
	Block: PMemoryBlocHeader;
	CurrentLeak: TLeak;
	i: integer;
	NewLeak: Boolean;
begin
	StoppedDueToMaxLeak := False;
	Block := LastBlock;
	while (Block <> nil) and (LeaksList.Count < MaxNumberOfLeaks) do
		begin
			if not MemoryBlockFreed(Block) then
				{this is a leak}
				begin
					CurrentLeak := TLeak.Create(Block);

					if IdentifyObjectFields then
						for i := 0 to NotDestroyedFieldsCount - 1 do
							if pointer(NotDestroyedFields[i]) = Block then
								begin
									CurrentLeak.fWasFieldOfAnotherObject := True;
									CurrentLeak.fOwnerClassName := TFieldInfo(NotDestroyedFieldsInfos[i]).OwnerClass.ClassName;
									CurrentLeak.fOtherFieldIndex := TFieldInfo(NotDestroyedFieldsInfos[i]).FieldIndex;
									CurrentLeak.fOtherIsDestroyed := True;
								end;

					//A future improvement: identify fields of not destroyed objects

					NewLeak := True;
					i := 0;
					while i < LeaksList.Count do
						begin
							if LeaksList.Item(i).IsEqual(CurrentLeak) then
								begin
									CurrentLeak.Destroy;
									CurrentLeak := LeaksList.Item(i);
									CurrentLeak.AddOccurence;
									i := LeaksList.Count;
									NewLeak := False;
								end;

							i := i + 1;
						end;

					if NewLeak then
						begin
							CurrentLeak.fID := LeaksList.Count;
							LeaksList.Add(CurrentLeak);
						end;

					ChronogicalInfo.Add(CurrentLeak);
				end;

			Block := Block.PreceedingBlock;
		end;

	if LeaksList.Count = MaxNumberOfLeaks then
		StoppedDueToMaxLeak := True;
end;

procedure GetBadBlocks(const B: TBlockList; const MaxNumberOfBlocks, MaxBlockSize: integer; var StoppedDueToMaxBlock: Boolean);
var
	Block: PMemoryBlocHeader;
begin
	StoppedDueToMaxBlock := False;
	Block := LastBlock;
	while (Block <> nil) and (B.Count < MaxNumberOfBlocks) do
		begin
			if MemoryBlockFreed(Block) and (Block.AllocatedSize > 5) and (Block.AllocatedSize <= MaxBlockSize) and (not IsMemFilledWithChar(PAnsiChar(Block) + SizeOf(TMemoryBlocHeader) + 4, Block.AllocatedSize - 5, CharToUseToWipeOut)) then
				B.Add(Block);

			Block := Block.PreceedingBlock;
		end;

	if B.Count = MaxNumberOfBlocks then
		StoppedDueToMaxBlock := True;
end;

procedure OutputAllCollectedInformation;
var
	OutputFile: Text;
	LeaksList: TLeakList;			   //Contains all instances of TLeak
	ChronogicalInfo: TLeakList;		 //Contains one ore more instance of each TLeak
	StoppedDueToMax: Boolean;
	TotalLeak: integer;
	i, j: integer;
	LastDisplayedTimeStamp: integer;
	BadBlocks: TBlockList;
  ChkLogFileName: string;
  NotePadStr : AnsiString;
begin
	InitializeOnce;
	UnMemChk;

  if ignoreLeakMsg then exit;
 	ChkLogFileName := ChangeFileExt(ParamStr(0), MemCheckLogFileNameSuffix);

	if FileExists(ChkLogFileName) then
		DeleteFile(ChkLogFileName);

	LeaksList := TLeakList.Create;
	ChronogicalInfo := TLeakList.Create;

	GetLeaks(LeaksList, ChronogicalInfo, MaxLeak, StoppedDueToMax);  //We collect the list of allocated blocks

	TotalLeak := 0;
	for i := 0 to ChronogicalInfo.Count - 1 do
		TotalLeak := TotalLeak + ChronogicalInfo.Item(i).Block.AllocatedSize;

  if (LeaksList.Count = 0) and (TotalLeak = 0) then begin
  	LeaksList.Destroy;
    ChronogicalInfo.Destroy;
    exit;
  end;


	//Prepare the output file
	if (IOResult <> 0) then ;	//Clears the internal IO error flag
	AssignFile(OutputFile, ChkLogFileName);
	Rewrite(OutputFile);
	WriteLn(OutputFile, OutputFileHeader);

	//Improve the header
	if StoppedDueToMax then
		WriteLn(OutputFile, 'Total leak not accurate due to MaxLeak constant reached, but at least ', TotalLeak, ' bytes'#13#10)
	else
		WriteLn(OutputFile, 'Total leak: ', TotalLeak, ' bytes'#13#10);

	//We output the memory leaks
	WriteLn(OutputFile, #13#10'*** MEMCHK: Blocks STILL allocated ***'#13#10);
	for i := 0 to LeaksList.Count - 1 do
		LeaksList.Item(i).OutputToFile(OutputFile);
	WriteLn(OutputFile, '*** MEMCHK: End of allocated blocks ***'#13#10);

	//We give chronological info
	WriteLn(OutputFile, #13#10'*** MEMCHK: Chronological leak information ***'#13#10);
	if TimeStampsCount > 0 then
		WriteLn(OutputFile, '  Time stamp: "', TimeStamps[0], '"');
	LastDisplayedTimeStamp := 0;
	for i := ChronogicalInfo.Count - 1 downto 0 do
		begin
			if (TimeStampsCount > 0) and (ChronogicalInfo.Item(i).Block.LastTimeStamp > LastDisplayedTimeStamp) then
				begin
					for j := LastDisplayedTimeStamp + 1 to ChronogicalInfo.Item(i).Block.LastTimeStamp do
						WriteLn(OutputFile, '  Time stamp: "', TimeStamps[j], '"');
					LastDisplayedTimeStamp := ChronogicalInfo.Item(i).Block.LastTimeStamp;
				end;
			ChronogicalInfo.Item(i).OutputOneLineToFile(OutputFile);
		end;
	for j := LastDisplayedTimeStamp + 1 to TimeStampsCount - 1 do
		WriteLn(OutputFile, '  Time stamp: "', TimeStamps[j], '"');
	WriteLn(OutputFile, #13#10'*** MEMCHK: End of chronological leak information ***'#13#10);

	//Output the allocation stats if necessary
	if CollectStatsAboutObjectAllocation then
		begin
			WriteLn(OutputFile, #13#10'*** MEMCHK: Allocation stats ***'#13#10);
			if TotalLeak > 0 then
				WriteLn(OutputFile, #9'The information is not accurate since there are memory leaks'#13#10);
			WriteLn(OutputFile, #9'Nb instances'#9'Instance size'#9'ClassName');
			for i := 0 to AllocStatsCount - 1 do
				WriteLn(OutputFile, #9, AllocatedInstances[i], #9#9, AllocatedObjectsClasses[i].InstanceSize, #9#9, AllocatedObjectsClasses[i].ClassName);
			WriteLn(OutputFile, #13#10'*** MEMCHK: End of allocation stats ***'#13#10);
		end;
	if ComputeMemoryUsageStats then
		begin
			WriteLn(OutputFile, #13#10'*** MEMCHK: Memory usage stats ***'#13#10);
			for i := 0 to MemoryUsageStatsCount - 1 do
				WriteLn(OutputFile, #9, MemoryUsageStats[i]);
			WriteLn(OutputFile, #13#10'*** MEMCHK: End of memory usage stats ***'#13#10);
		end;
	if KeepMaxMemoryUsage then
		WriteLn(OutputFile, #13#10'*** Biggest memory usage was: ', MaxMemoryUsage, ' ***' + #13#10#13#10#13#10);

	//Get and output the damaged blocks if necessary
	BadBlocks := TBlockList.Create;
	if CheckWipedBlocksOnTermination then
		begin
			GetBadBlocks(BadBlocks, MaxLeak, DoNotCheckWipedBlocksBiggerThan, StoppedDueToMax);

			WriteLn(OutputFile, #13#10'*** MEMCHK: Blocks written to after destruction ***'#13#10);
			if StoppedDueToMax then
				WriteLn(OutputFile, #9'Number of bad blocks not accurate  due to MaxLeak constant reached, but at least ', BadBlocks.Count, #13#10)
			else
				WriteLn(OutputFile, #9'Bad blocks count: ', BadBlocks.Count, #13#10);

			for i := 0 to BadBlocks.Count - 1 do
				begin
					WriteLn(OutputFile, #9'* Destroyed block damaged');
					WriteLn(OutputFile, #9#9'Call stack at allocation time:');
					Write(OutputFile, CallStackTextualRepresentation(BadBlocks.Item(i).CallerAddress, #9#9#9));
					WriteLn(OutputFile, #9#9'Destroyed at: ', GetDebugInfoAt(Cardinal(BadBlocks.Item(i).DestructionAdress)));
				end;

			WriteLn(OutputFile, #13#10'*** MEMCHK: End of blocks written to after destruction ***'#13#10);
		end;
	BadBlocks.Destroy;

	//Save and display the output file
	Close(OutputFile);

	if ShowLogFile or CollectStatsAboutObjectAllocation or ComputeMemoryUsageStats or KeepMaxMemoryUsage then begin
    NotePadStr := AnsiString(NotepadApp + ' ' + ChkLogFileName);
    WinExec(PAnsiChar(NotePadStr), sw_Show);
  end;


	//Release the memory
	for i := 0 to LeaksList.Count - 1 do
		LeaksList.Item(i).Destroy;
	LeaksList.Destroy;
	ChronogicalInfo.Destroy;
end;

//---------------------------------------------------
procedure AddTimeStampInformation(const I: string);
begin
	InitializeOnce;

	if TimeStampsCount = TimeStampsAllocated then
		begin
			if TimeStampsAllocated = 0 then
				TimeStampsAllocated := 10;
			TimeStampsAllocated := TimeStampsAllocated * 2;

			UnMemChk;
			ReallocMem(TimeStamps, TimeStampsAllocated * sizeof(WideString));
			ZeroMemory(pointer(integer(TimeStamps) + TimeStampsCount * sizeof(WideString)), (TimeStampsAllocated - TimeStampsCount) * SizeOf(WideString));
			MemChk;
		end;

	TimeStamps[TimeStampsCount] := I + ' (Time stamp: ' + IntToStr(TimeStampsCount) + ')';
	TimeStampsCount := TimeStampsCount + 1;
end;


//---------------------------------------------------
function DefAllocMem(Size: NativeInt): Pointer;
begin
  GetMem(Result, Size);  //result := SysAllocMem(Size);
  if (Result <> nil) then
    FillChar(Result^, Size, 0);
end;


//---------------------------------------------------
procedure MemChk;
const
	LeakTrackingMemoryManager: TMemoryManagerEx = (
		GetMem: LeakTrackingGetMem;
		FreeMem: LeakTrackingFreeMem;
		ReallocMem: LeakTrackingReallocMem;

    AllocMem: DefAllocMem;
    RegisterExpectedMemoryLeak: InvalidRegisterAndUnRegisterMemoryLeak;
    UnRegisterExpectedMemoryLeak: InvalidRegisterAndUnRegisterMemoryLeak;
		);


	HeapCheckingMemoryManager: TMemoryManagerEx = (
		GetMem: HeapCheckingGetMem;
		FreeMem: HeapCheckingFreeMem;
		ReallocMem: HeapCheckingReallocMem;
    AllocMem: DefAllocMem;
    RegisterExpectedMemoryLeak: InvalidRegisterAndUnRegisterMemoryLeak;
    UnRegisterExpectedMemoryLeak: InvalidRegisterAndUnRegisterMemoryLeak;
		);
begin
  AddressOfTCustomFormCreate := @TCustomForm.Create;
  AddressOfTGlassFrameCreate := @TGlassFrame.Create;

	assert(sizeof(TMemoryBlocHeader) mod 8 = 0, 'SizeOf(TMemoryBlocHeader) in MemCheck should be a multiple of 8');

	if not MemCheckActive then
		begin
			InitializeOnce;
			if CheckHeapStatus then begin
				SetMemoryManager(HeapCheckingMemoryManager);
				UpdateLastHeapStatus;
			end	else
				SetMemoryManager(LeakTrackingMemoryManager);
			MemCheckActive := True;
		end;
end;

//------------------------------------------------------
procedure CommitReleases;
var	Block, BlockToFree, previous: PMemoryBlocHeader;
begin
	InitializeOnce;

	Block := LastBlock;
	Previous := nil;

	while Block <> nil do	begin
    BlockToFree := Block;
    Block := Block.PreceedingBlock;

    if MemoryBlockFreed(BlockToFree) then begin
        if LastBlock = BlockToFree then
          LastBlock := Block;

        if previous <> nil then
          previous.PreceedingBlock := Block;

        OldMemoryManager.FreeMem(BlockToFree);
    end else
      previous := BlockToFree;
  end;
end;

//------------------------------------------------------------------------------------------------
function CallStackTextualRepresentation(const S: TCallStack; const LineHeader: string): string;
var	i: integer;
begin
	i := 0;
	Result := '';

	while (i < StoredCallStackDepth) and (S[i] <> nil) do begin
  	Result := Result + LineHeader + 'call stack - ' + IntToStr(i) + ' : ' + GetDebugInfoAt(Cardinal(S[i])) + #13#10;
		i := i + 1;
	end;
end;

//------------------------------------------------------------------------------------------------
procedure SetDispl;
var
	NTHeader: PImageFileHeader;                            //JmLin rs102
	NTOptHeader: PImageOptionalHeader32;//PImageOptionalHeader;
begin
	NTHeader := PImageFileHeader(Cardinal(PImageDosHeader(HInstance)._lfanew) + HInstance + 4); {SizeOf(IMAGE_NT_SIGNATURE) = 4}
	NTOptHeader := PImageOptionalHeader32(Cardinal(NTHeader) + IMAGE_SIZEOF_FILE_HEADER);
	DisplOfExe := HInstance + NTOptHeader.BaseOfCode;
	//Result := HInstance + PImageNtHeaders(LongInt(HInstance)+PImageDosHeader(HInstance)^._lfanew)^.OptionalHeader.BaseOfCode;
end;

//------------------------------------------------------------------------------------------------
function CardinalToHexa(i: Cardinal): string;
const	HexChars: array[0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var	J: integer;
begin
	Result := '';
	for j := 1 to 8 do begin
    Result := HexChars[i and $0F] + Result;
    I := I shr 4;
  end;
end;

//------------------------------------------------------------------------------------------------
function GetDebugInfoAt(const TheAddress: Cardinal): string;
var	U: TUnitDebugInfos;
	  AddressInDebugInfos: Cardinal;
begin
	//InitializeOnce;  //already called in MemChk;
	if UseDebugInfos and (TheAddress > DisplOfExe) then begin
    AddressInDebugInfos := TheAddress - DisplOfExe;
    U := UnitWhichContainsAddress(AddressInDebugInfos);
    if U <> nil then
      Result := 'Module ' + U.Name + RoutineWhichContainsAddress(AddressInDebugInfos) + U.LineWhichContainsAddress(AddressInDebugInfos)
    else
      Result := RoutineWhichContainsAddress(AddressInDebugInfos);
	end	else
		Result := NoDebugInfo;

	Result := Result + ' : ' + CardinalToHexa(TheAddress);
end;


                                         initialization
//------------------------------------------------------------------------------------------------
//InitContext.InitTable^.UnitInfo array stores all units entrance pointer.
//in system unit, TProc(P)() call is translated as assembly instruction of call [esi]
//so, esi stores the the offset address of memcheck.memcheck.

//System.InitContext.InitTable^.UnitInfo is unfortunately not public,
//Code address calculation trick is used here to get the reference of it

{
System.pas:
--------------------
procedure InitUnits;
begin
    ...
    while I < Count do begin
      P := Table^[I].Init;
      Inc(I);
      InitContext.InitCount := I;                                     $0053AE83  Mov [@InitContext.InitCount], ebx
      if Assigned(P) and Assigned(Pointer(P^)) then begin
        TProc(P)();                                                   $0053AE94: call esi
        ...
      end;
    end;
end;
}

//Checked 27 July 23 For RS10.2
//------------------------------------------------------------------------------------------------
asm
  push eax;
  mov memchkPtr, esi;             //esi stores the the offset address of memcheck.memcheck.
  mov  eax, [ebp + 4];            //address of caller's next instruction e.g. 0040AA20
  sub eax, $0040AA20 - ($0040AA0F + 2); //get address of InitContext.InitCount: 0040AA20 0040AA0F+2, (mov  [InitContext.InitCount], ebx) in procedure InitUnits;
  mov eax, [eax];                 //now eax is the address of InitContext.InitCount
  sub eax, 4;                     //InitContext.InitCount is defined next to InitContext.InitTable
  mov eax, [eax];                 //take the address of InitContext.InitTable
  mov UnitsInfo, eax;
  pop eax;
end;

                                       finalization
//----------------------------------------------------------------------------------------------------------------------------------------
if UseMemChkException then // ExceptProc is reset to nil in SysUtils.DoneExceptions. Original : ExceptProc = @MyExceptProc then
  Close(SevereExceptionsLogFile);

if MemCheckInitialized then	begin
  if MemCheckActive then begin
    UnMemChk;
    OutputAllCollectedInformation;
    GoThroughAllocatedBlocks;
  end;

  if CheckHeapStatus then
    HeapStatusSynchro.Destroy;
  LinkedListSynchro.Destroy;
  FreeMem(TimeStamps);
  FreeMem(AllocatedInstances);
  OutOfMemory.Destroy;
end;


end.

