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

unit ledger.standard;

{$mode delphi}

interface

uses
  ledger;

type

  IIntLedger = ILedger<Integer>;
  ISingleLedger = ILedger<Single>;
  IDoubleLedger = ILedger<Double>;
  IExtendedLedger = ILedger<Extended>;

  TIntLedgerEntries = TLedgerEntryArray<Integer>;
  TSingleLedgerEntries = TLedgerEntryArray<Single>;
  TDoubleLedgerEntries = TLedgerEntryArray<Double>;
  TExtendedLedgerEntries = TLedgerEntryArray<Extended>;

  TIntFilter = TFilter<Integer>;
  TSingleFilter = TFilter<Single>;
  TDoubleFilter = TFilter<Double>;
  TExtendedFilter = TFilter<Extended>;

  { TNumericLedger }

  TNumericLedger<T> = class(TLedgerImpl<T>)
  strict protected
    function DoGetEmptyBalance: T; override;
    function DoGetPositive(const AEntry: T): Boolean; override;
  end;

  TIntLedgerImpl = class(TNumericLedger<Integer>)
  end;

  TSingleLedgerImpl = class(TNumericLedger<Single>)
  end;

  TDoubleLedgerImpl = class(TNumericLedger<Double>)
  end;

  TExtendedLedgerImpl = class(TNumericLedger<Extended>)
  end;

function NewIntLedger : IIntLedger;
function NewSingleLedger : ISingleLedger;
function NewDoubleLedger : IDoubleLedger;
function NewExtendedLedger : IExtendedLedger;

implementation

function NewIntLedger : IIntLedger;
begin
  Result:=TIntLedgerImpl.Create;
end;

function NewSingleLedger : ISingleLedger;
begin
  Result:=TSingleLedgerImpl.Create;
end;

function NewDoubleLedger : IDoubleLedger;
begin
  Result:=TDoubleLedgerImpl.Create;
end;

function NewExtendedLedger : IExtendedLedger;
begin
  Result:=TExtendedLedgerImpl.Create;
end;

{ TNumericLedger }

function TNumericLedger<T>.DoGetEmptyBalance: T;
begin
  Result:=0;
end;

function TNumericLedger<T>.DoGetPositive(const AEntry: T): Boolean;
begin
  Result:=AEntry>0;
end;

end.

