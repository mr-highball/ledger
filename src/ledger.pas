{ ledger

  Copyright (c) 2018 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

unit ledger;

{$mode delphi}{$H+}

interface

uses
  syncobjs, fgl;

var
  Critical : TCriticalSection;

type

  TLedgerType = (
    ltCredit,
    ltDebit
  );

  TLedgerTypes = set of TLedgerType;

  TLedgerEntry<T> = packed record
  private
    FEntry : T;
    FID : String;
    FType : TLedgerType;
    FTimeStamp : TDateTIme;
  public
    property ID : String read FID write FID;
    property Entry : T read FEntry write FEntry;
    property LedgerType : TLedgerType read FType write FType;
    property Timestamp : TDateTime read FTimestamp write FTimeStamp;
  end;

  TLedgerEntryArray<T> = array of TLedgerEntry<T>;

  TFilter<T> = function(Const AEntry:T;Const AType:TLedgerType):Boolean;

  { ILedger }

  ILedger<T> = interface
    ['{751BB67C-AA79-40ED-8A11-8034EE997884}']
    //property methods
    function GetBalance: T;
    function GetCount(const ATypes: TLedgerTypes): Cardinal;
    function GetCredits: TLedgerEntryArray<T>;
    function GetDebits: TLedgerEntryArray<T>;
    function GetEntries(const AID: String): TLedgerEntry<T>;
    //properties
    property Balance : T read GetBalance;
    property Credits : TLedgerEntryArray<T> read GetCredits;
    property Debits : TLedgerEntryArray<T> read GetDebits;
    property Entries[Const AID:String] : TLedgerEntry<T>
      read GetEntries;default;
    property Count[Const ATypes:TLedgerTypes] : Cardinal read GetCount;
    //methods
    function RecordEntry(Const AEntry:T;Const AType:TLedgerType;
      Const ATimestamp:TDateTime; Out ID:String):ILedger<T>;overload;
    function RecordEntry(Const AEntry:T;Const AType:TLedgerType;
      Out ID:String):ILedger<T>;overload;
    function RecordEntry(Const AEntry:T;
      Const AType:TLedgerType):ILedger<T>;overload;
    function RecordEntries(Const AEntries:TLedgerEntryArray<T>):ILedger<T>;
    function Flatten : ILedger<T>;
    function Filter(Const AFilter:TFilter<T>;
      Out Entries:TLedgerEntryArray<T>):ILedger<T>;
    function Clear(Const AType:TLedgerType):ILedger<T>;overload;
    function Clear:ILedger<T>;overload;
  end;

  { ILedgerImpl }

  TLedgerImpl<T> = class(TInterfacedObject,ILedger<T>)
  strict private
    type
      { TEntryPair }
      TEntryPair = packed record
      private
        FIndex : Integer;
        FType : TLedgerType;
      public
        property Index : Integer read FIndex write FIndex;
        property LedgerType : TLedgerType read FType write FType;
      end;
      TMap = TFPGMap<String,TEntryPair>;
  strict private
    FLastBalance : T;
    FCredits : TLedgerEntryArray<T>;
    FDebits : TLedgerEntryArray<T>;
    FMap : TMap;
    function GetBalance: T;
    function GetCount(const ATypes: TLedgerTypes): Cardinal;
    function GetCredits: TLedgerEntryArray<T>;
    function GetDebits: TLedgerEntryArray<T>;
    function GetEntries(const AID: String): TLedgerEntry<T>;
    procedure Rebalance;
  strict protected
    procedure DoBeforeRecord(Const AEntry:T;Const AType:TLedgerType);virtual;
    procedure DoAfterRecord(Const AEntry:T;Const AType:TLedgerType;
      Const ABalance:T;Const AID:String);virtual;
    procedure DoBeforeClear(Const AType:TLedgerType);virtual;
    procedure DoAfterClear(Const AType:TLedgerType);virtual;
    function DoGetEmptyBalance:T;virtual;
    function DoGetPositive(Const AEntry:T):Boolean;virtual;abstract;
  public
    property Balance : T read GetBalance;
    property Credits : TLedgerEntryArray<T> read GetCredits;
    property Debits : TLedgerEntryArray<T> read GetDebits;
    property Entries[Const AID:String] : TLedgerEntry<T>
      read GetEntries;default;
    property Count[Const ATypes:TLedgerTypes] : Cardinal read GetCount;
    function RecordEntry(Const AEntry:T;Const AType:TLedgerType;
      Const ATimestamp:TDateTime; Out ID:String):ILedger<T>;overload;
    function RecordEntry(Const AEntry:T;Const AType:TLedgerType;
      Out ID:String):ILedger<T>;overload;
    function RecordEntry(Const AEntry:T;
      Const AType:TLedgerType):ILedger<T>;overload;
    function RecordEntries(Const AEntries:TLedgerEntryArray<T>):ILedger<T>;
    function Flatten : ILedger<T>;
    function Filter(Const AFilter:TFilter<T>;
      Out Entries:TLedgerEntryArray<T>):ILedger<T>;
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

function TLedgerImpl<T>.GetCount(const ATypes: TLedgerTypes): Cardinal;
begin
  if (ATypes=[]) or (ATypes=[ltCredit,ltDebit]) then
    Result:=Length(FCredits) + Length(FDebits)
  else if ATypes=[ltCredit] then
    Result:=Length(FCredits)
  else
    Result:=Length(FDebits);
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
var
  I:Integer;
  LPair:TEntryPair;
begin
  Critical.Enter;
  try
    if not FMap.Sorted then
      FMap.Sort;
    if not FMap.Find(AID,I) then
      raise Exception.Create(AID + ' is not a valid ID');
    LPair:=FMap.Data[I];
    //now use the type of the pair to determine which array to use
    if LPair.LedgerType=ltCredit then
      Result:=FCredits[LPair.Index]
    else
      Result:=FDebits[LPair.Index];
  finally
    Critical.Leave;
  end;
end;

function TLedgerImpl<T>.RecordEntry(const AEntry: T; const AType: TLedgerType;
  const ATimestamp: TDateTime; out ID: String): ILedger<T>;
var
  LEntry:TLedgerEntry<T>;
begin
  Result:=Self as ILedger<T>;
  //set local record
  LEntry.ID:=TGuid.NewGuid().ToString();
  LEntry.TimeStamp:=ATimeStamp;
  LEntry.Entry:=AEntry;
  LEntry.LedgerType:=AType;
  Critical.Enter;
  try
    try
      //depending on type, put record in proper array
      if AType=ltCredit then
      begin
        SetLength(FCredits,Succ(Length(FCredits)));
        FCredits[High(FCredits)]:=LEntry;
        FLastBalance:=FLastBalance + AEntry;
      end
      else
      begin
        SetLength(FDebits,Succ(Length(FDebits)));
        FDebits[High(FDebits)]:=LEntry;
        FLastBalance:=FLastBalance - AEntry;
      end;
      ID:=LEntry.ID;
    except on E:Exception do
      raise E;
    end;
  finally
    Critical.Leave;
  end;
end;

function TLedgerImpl<T>.RecordEntry(const AEntry: T; const AType: TLedgerType;
  out ID: String): ILedger<T>;
begin
  Result:=RecordEntry(AEntry,AType,Now,ID);
end;

function TLedgerImpl<T>.RecordEntry(Const AEntry:T;
  Const AType:TLedgerType):ILedger<T>;
var
  LID:String;
begin
  Result:=RecordEntry(AEntry,AType,LID);
end;

function TLedgerImpl<T>.RecordEntries(
  const AEntries: TLedgerEntryArray<T>): ILedger<T>;
var
  I:Integer;
  LEntry:TLedgerEntry<T>;
  LPair:TEntryPair;
begin
  Result:=Self as ILedger<T>;
  if not FMap.Sorted then
    FMap.Sort;
  for I:=0 to High(AEntries) do
  begin
    LEntry:=AEntries[I];
    //allow caller to assign id's, but we have to make sure they are unique
    if (LEntry.ID.IsEmpty) or (FMap.IndexOf(LEntry.ID)>=0) then
      LEntry.ID:=TGUID.NewGuid.ToString;
    LPair.LedgerType:=LEntry.LedgerType;
    Critical.Enter;
    try
      //use corresponding array for type
      if AEntries[I].LedgerType=ltCredit then
      begin
        SetLength(FCredits,Succ(Length(FCredits)));
        FCredits[High(FCredits)]:=LEntry;
        LPair.Index:=High(FCredits);
      end
      else
      begin
        SetLength(FDebits,Succ(Length(FDebits)));
        FDebits[High(FDebits)]:=LEntry;
        LPair.Index:=High(FDebits);
      end;
      //update the map
      FMap.AddOrSetData(LEntry.ID,LPair);
    finally
      Critical.Leave;
    end;
  end;
  //now rebalance after adding everything
  Rebalance;
end;

function TLedgerImpl<T>.Flatten: ILedger<T>;
var
  LEntry:T;
  LType:TLedgerType;
  LID:String;
begin
  Critical.Enter;
  try
    LEntry:=FLastBalance;
    Clear;
    if DoGetPositive(FLastBalance) then
      LType:=ltCredit
    else
      LType:=ltDebit;
    Result:=RecordEntry(LEntry,LType,Now,LID);
  finally
    Critical.Leave;
  end;
end;

function TLedgerImpl<T>.Clear(const AType: TLedgerType):ILedger<T>;
var
  I:Integer;
begin
  Result:=Self as ILedger<T>;
  DoBeforeClear(AType);
  Critical.Enter;
  try
    if AType=ltCredit then
    begin
      for I:=0 to High(FCredits) do
        FMap.Remove(FCredits[I].ID);
      SetLength(FCredits,0)
    end
    else
    begin
      for I:=0 to High(FDebits) do
        FMap.Remove(FCredits[I].ID);
      SetLength(FDebits,0);
    end;
  finally
    Critical.Leave;
  end;
  DoAfterClear(AType);
end;

function TLedgerImpl<T>.Clear:ILedger<T>;
begin
  Clear(ltCredit);
  Result:=Clear(ltDebit);
end;

procedure TLedgerImpl<T>.DoBeforeRecord(Const AEntry:T;Const AType:TLedgerType);
begin
end;

procedure TLedgerImpl<T>.DoAfterRecord(Const AEntry:T;Const AType:TLedgerType;
  Const ABalance:T;Const AID:String);
begin
end;

procedure TLedgerImpl<T>.DoBeforeClear(Const AType:TLedgerType);
begin
end;

procedure TLedgerImpl<T>.DoAfterClear(Const AType:TLedgerType);
begin
end;

procedure TLedgerImpl<T>.Rebalance;
var
  I:Integer;
begin
  Critical.Enter;
  try
    //first set balance to empty
    FLastBalance:=DoGetEmptyBalance;
    //sum all of the credits and debits
    for I:=0 to High(FCredits) do
      FLastBalance:=FLastBalance + FCredits[I].Entry;
    for I:=0 to High(FDebits) do
      FLastBalance:=FLastBalance - FDebits[I].Entry;
  finally
    Critical.Leave;
  end;
end;

function TLedgerImpl<T>.DoGetEmptyBalance:T;
begin
end;

function TLedgerImpl<T>.Filter(Const AFilter:TFilter<T>;
  Out Entries:TLedgerEntryArray<T>):ILedger<T>;
var
  I:Integer;
begin
  Result:=Self as ILedger<T>;
  if not Assigned(AFilter) then
    Exit;
  Critical.Enter;
  try
    //filter credits
    for I:=0 to High(FCredits) do
    begin
      if AFilter(FCredits[I].Entry,FCredits[I].LedgerType) then
      begin
        SetLength(Entries,Succ(Length(Entries)));
        Entries[High(Entries)]:=FCredits[I];
      end;
    end;
    //filter debits
    for I:=0 to High(FDebits) do
    begin
      if AFilter(FDebits[I].Entry,FDebits[I].LedgerType) then
      begin
        SetLength(Entries,Succ(Length(Entries)));
        Entries[High(Entries)]:=FDebits[I];
      end;
    end;
  finally
    Critical.Leave;
  end;
end;

constructor TLedgerImpl<T>.Create;
begin
  FMap:=TMap.Create;
  FLastBalance:=DoGetEmptyBalance;
end;

destructor TLedgerImpl<T>.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

initialization
  Critical:=TCriticalSection.Create;
finalization
  Critical.Free;
end.

