# ledger
generic ledger implementation for basic accounting

To request features or report a bug, open a github issue with details/steps to reproduce

# How To Use

1. download and install lazarus if you don't already have it (http://www.lazarus-ide.org)
1. git clone this repo
1. open ledger_test.lpr and attempt to compile/run (F9 Key)
    * this project shows some basic usage of the library
    * also, by going to `Toolbar -> Project\Project Options\Paths` you can copy the `other units` text to include in your own project
1. add `.\src` path to your project `other units`

# Custom Processing

There are some protected virtual methods that can be overridden

```
procedure DoBeforeRecord(Const AEntry:T;Const AType:TLedgerType);virtual;
procedure DoAfterRecord(Const AEntry:T;Const AType:TLedgerType;
  Const ABalance:T;Const AID:String);virtual;
procedure DoBeforeClear(Const AType:TLedgerType);virtual;
procedure DoAfterClear(Const AType:TLedgerType);virtual;
```

**Tip Jar**
  * :dollar: BTC - bc1q55qh7xptfgkp087sfr5ppfkqe2jpaa59s8u2lz
  * :euro: LTC - LPbvTsFDZ6EdaLRhsvwbxcSfeUv1eZWGP6
