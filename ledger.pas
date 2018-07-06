unit ledger;

{$mode delphi}{$H+}

interface

uses
  syncobjs;

type

  TLedgerType = (
    ltCredit,
    ltDebit
  );

  TLedgerTypes = set of TLedgerType;

  TLedgerEntry<T> = packed record
  private
    FAmount : T;
    FID : String;
    FType : TLedgerType;
    FTimeStamp : TDateTIme;
  public
    property ID : String read FID write FID;
    property Amount : T read FAmount write FAmount;
    property LedgerType : TLedgerType read FType write FType;
    property Timestamp : TDateTime read FTimestamp write FTimeStamp;
  end;

  TLedgerEntryArray<T> = array of TLedgerEntry<T>;

  { ILedger }

  ILedger<T> = interface
    ['{751BB67C-AA79-40ED-8A11-8034EE997884}']
    //property methods
    function GetBalance: T;
    function GetCount(const AType: TLedgerTypes): Cardinal;
    function GetCredits: TLedgerEntryArray<T>;
    function GetDebits: TLedgerEntryArray<T>;
    function GetEntries(const AID: String): TLedgerEntry<T>;
    //properties
    property Balance : T read GetBalance;
    property Credits : TLedgerEntryArray<T> read GetCredits;
    property Debits : TLedgerEntryArray<T> read GetDebits;
    property Entries[Const AID:String] : TLedgerEntry<T>
      read GetEntries;default;
    property Count[Const AType:TLedgerTypes] : Cardinal read GetCount;
    //methods
    function RecordEntry(Const AAmount:T;Const AType:TLedgerType;
      Const ATimestamp:TDateTime; Out ID:String):ILedger<T>;overload;
    function RecordEntry(Const AAmount:T;Const AType:TLedgerType;
      Out ID:String):ILedger<T>;overload;
    function RecordEntries(Const AEntries:TLedgerEntryArray<T>):ILedger<T>;
    function Flatten : ILedger<T>;
    function Clear(Const AType:TLedgerType):ILedger<T>;overload;
    function Clear:ILedger<T>;overload;
  end;

  { ILedgerImpl }

  TLedgerImpl<T> = class(TInterfacedObject,ILedger<T>)
  strict private
    FLastBalance : T;
    FCredits : TLedgerEntryArray<T>;
    FDebits : TLedgerEntryArray<T>;
    FCritical : TCriticalSection;
    function GetBalance: T;
    function GetCount(const AType: TLedgerTypes): Cardinal;
    function GetCredits: TLedgerEntryArray<T>;
    function GetDebits: TLedgerEntryArray<T>;
    function GetEntries(const AID: String): TLedgerEntry<T>;
  strict protected
    procedure DoBeforeRecord(Const AAmount:T;Const AType:TLedgerType);virtual;
    procedure DoAfterRecord(Const AAmount:T;Const AType:TLedgerType;
      Const ABalance:T;Const AID:String);virtual;
    procedure DoBeforeClear(Const AType:TLedgerType);virtual;
    procedure DoAfterClear(Const AType:TLedgerType);virtual;
  public
    property Balance : T read GetBalance;
    property Credits : TLedgerEntryArray<T> read GetCredits;
    property Debits : TLedgerEntryArray<T> read GetDebits;
    property Entries[Const AID:String] : TLedgerEntry<T>
      read GetEntries;default;
    property Count[Const AType:TLedgerTypes] : Cardinal read GetCount;
    function RecordEntry(Const AAmount:T;Const AType:TLedgerType;
      Const ATimestamp:TDateTime; Out ID:String):ILedger<T>;overload;
    function RecordEntry(Const AAmount:T;Const AType:TLedgerType;
      Out ID:String):ILedger<T>;overload;
    function RecordEntries(Const AEntries:TLedgerEntryArray<T>):ILedger<T>;
    function Flatten : ILedger<T>;
    function Clear(Const AType:TLedgerType):ILedger<T>;overload;
    function Clear:ILedger<T>;overload;
    constructor Create;virtual;
    destructor Destroy;override;
  end;

implementation
uses
  SysUtils;

{ TLedgerImpl }

function TLedgerImpl<T>.GetBalance: T;
begin
  Result:=FLastBalance;
end;

function TLedgerImpl<T>.GetCount(const AType: TLedgerTypes): Cardinal;
begin
  Result:=Length(FCredits) + Length(FDebits);
end;

function TLedgerImpl<T>.GetCredits: TLedgerEntryArray;
begin
  Result:=FCredits;
end;

function TLedgerImpl<T>.GetDebits: TLedgerEntryArray;
begin
  Result:=FDebits;
end;

function TLedgerImpl<T>.GetEntries(const AID: String): TLedgerEntry;
begin
  //todo - look in map for particular index of id
end;

function TLedgerImpl<T>.RecordEntry(const AAmount: T; const AType: TLedgerType;
  const ATimestamp: TDateTime; out ID: String): ILedger<T>;
var
  LEntry:TLedgerEntry<T>;
  I:Integer;
begin
  Result:=Self as ILedger<T>;
  //set local record
  LEntry.ID:=TGuid.NewGuid().ToString();
  LEntry.TimeStamp:=ATimeStamp;
  LEntry.Amount:=AAmount;
  LEntry.LedgerType:=AType;
  FCritical.Enter;
  try
    try
      //depending on type, put record in proper array
      if AType=ltCredit then
      begin
        SetLength(FCredits,Succ(Length(FCredits)));
        FCredits[High(FCredits)]:=LEntry;
        FLastBalance:=FLastBalance + AAmount;
      end
      else
      begin
        SetLength(FDebits,Succ(Length(FDebits)));
        FDebits[High(FDebits)]:=LEntry;
        FLastBalance:=FLastBalance - AAmount;
      end;
      ID:=LEntry.ID;
    except on E:Exception do
      raise E;
    end;
  finally
    FCritical.Leave;
  end;
end;

function TLedgerImpl<T>.RecordEntry(const AAmount: T; const AType: TLedgerType;
  out ID: String): ILedger<T>;
begin
  Result:=RecordEntry(AAmount,AType,Now,ID);
end;

function TLedgerImpl<T>.RecordEntries(
  const AEntries: TLedgerEntryArray<T>): ILedger<T>;
begin
  Result:=Self as ILedger<T>;
  //todo - record a series of entries to their corresponding arrays/maps
  //and assign ids where duplicates exist
end;

function TLedgerImpl<T>.Flatten: ILedger<T>;
begin
  Result:=Self as ILedger<T>;
  //todo - record final balance as either a debit or credit and shrink
  //arrays and maps to 1 entry
end;

function TLedgerImpl<T>.Clear(const AType: TLedgerType):ILedger<T>;
begin
  Result:=Self as ILedger<T>;
  FCritical.Enter;
  try
    DoBeforeClear(AType);
    if AType=ltCredit then
    begin
      //todo - clear credit map
      SetLength(FCredits,0)
    end
    else
    begin
      //todo - clear debit map
      SetLength(FDebits,0);
    end;
    DoAfterClear(AType);
  finally
    FCritical.Leave;
  end;
end;

function TLedgerImpl<T>.Clear:ILedger<T>;
begin
  Clear(ltCredit);
  Result:=Clear(ltDebit);
end;

procedure TLedgerImpl<T>.DoBeforeRecord(Const AAmount:T;Const AType:TLedgerType);
begin
end;

procedure TLedgerImpl<T>.DoAfterRecord(Const AAmount:T;Const AType:TLedgerType;
  Const ABalance:T;Const AID:String);
begin
end;

procedure TLedgerImpl<T>.DoBeforeClear(Const AType:TLedgerType);
begin
end;

procedure TLedgerImpl<T>.DoAfterClear(Const AType:TLedgerType);
begin
end;

constructor TLedgerImpl<T>.Create;
begin
  FCritical:=TCriticalSection.Create;
end;

destructor TLedgerImpl<T>.Destroy;
begin
  FCritical.Free;
  inherited Destroy;
end;

end.

