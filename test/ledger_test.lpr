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

program ledger_test;

{$mode delphi}{$H+}

uses
  ledger,
  ledger.standard;

procedure TestSimple;
var
  LTest:IDoubleLedger;
begin
  WriteLn('TestSimple');
  //use helper method to get a double ledger, then
  //add 1 credit and 1 debit which should balance
  LTest:=NewDoubleLedger;
  WriteLn(LTest.RecordEntry(1,ltCredit).Balance);
  WriteLn(LTest.RecordEntry(1,ltDebit).Balance);
end;

(*
  this filter simply checks for entries that are recorded
  as debits (mimics the ILedger.Debits property)
*)
function FilterDebits(Const AEntry:Extended;
  Const AType:TLedgerType):Boolean;
begin
  Result:=False;
  if AType=ltDebit then
    Result:=True;
end;

procedure TestFilter;
var
  I:Integer;
  LTest:IExtendedLedger;
  LEntries:TExtendedLedgerEntries;
  LFilter:TExtendedFilter;
begin
  WriteLn('TestFilter');
  LTest:=NewExtendedLedger;
  //can either use a local var, or just use @methodname
  LFilter:=FilterDebits;
  WriteLn('Balance:',NewExtendedLedger
    .RecordEntry(1,ltCredit)
    .RecordEntry(1.1,ltDebit)
    .Filter(@FilterDebits,LEntries)
    .Balance
  );
  WriteLn('Debits using filter:');
  if Length(LEntries)<1 then
    WriteLn('(none)')
  else
    for I:=0 to High(LEntries) do
      WriteLn(LEntries[I].Entry);
end;

procedure TestClear;
begin
  WriteLn('TestClear - should be 0');
  WriteLn(NewIntLedger
    .RecordEntry(1,ltCredit)
    .Clear
    .Count[[]]
  );
end;
begin
  TestSimple;
  TestFilter;
  TestClear;
  ReadLn;
end.

