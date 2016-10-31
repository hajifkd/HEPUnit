BeginPackage["UnitMath`"]

unit::differentUnit = "Units are different";
unity = <|"GeV" -> {1, 1}|>;
coeff[unit[a_, _Association, ___]] := a;
unitTable[unit[_, b_Association, ___]] := b;
unit[a_, b_Association, ___] /; And @@ (# == 0 & /@ b) := a;
unit /: Format[unit[a_, b_Association, s_Association : <||>], 
   StandardForm] := With[{
    coeff = Times @@ (Lookup[s, #, {1}][[1]]^b[#] & /@ Keys[b]),
    names = 
     Association @@ (# -> Lookup[s, #, {None, #}][[2]] & /@ Keys[b])
    },
   ToString[N[a / coeff, 5], TraditionalForm] <> 
    StringJoin @@ (ToString[If[b[#] != 1, names[#]^b[#], names[#]], 
          StandardForm] <> " " & /@ Keys[b])
   ];

toNaturalUnit[unit[a_, b_Association, ___]] := With[{
    coeff = Times @@ (unity[#][[1]]^b[#] & /@ Keys[b]),
    exponent = Plus @@ (unity[#][[2]] b[#] & /@ Keys[b])
    },
   {a * coeff, exponent}
   ];

convert[from_unit, to : unit[_, a_Association, s___]] := Block[{
    f = toNaturalUnit[from],
    t = toNaturalUnit[to],
    cf := f[[1]],
    ef := f[[2]],
    ct := t[[1]],
    et := t[[2]]
    },
   unit[cf ct^(-(ef/et)), ef/et a, s]
   ];

unit /: Plus[unit[a_, b_Association, s___], unit[c_, b_, ___]] := 
  unit[a + c, b, s];
unit /: Plus[_unit, _unit] := (Message[unit::differentUnit]; $Failed);

unit /: Minus[unit[a_, b_Association, s___], unit[c_, b_, ___]] := 
  unit[a - c, b, s];
unit /: Minus[_unit, _unit] := (Message[unit::differentUnit]; $Failed);

unit /: Times[unit[a_, b_Association, s___], 
   unit[c_, d_Association, ___]] := Block[{
    keySet = Union[Keys[b], Keys[d]],
    assocList := # -> Lookup[b, #, 0] + Lookup[d, #, 0] & /@ keySet,
    assoc := Association @@ assocList
    },
   unit[a c, assoc, s]
   ];
unit /: Times[unit[a_, b_Association, s___], c_] := unit[a c, b, s];
unit /: Times[c_, unit[a_, b_Association, s___]] := unit[c a, b, s];

unit /: Power[unit[a_, b_Association, s___], c_] := 
  unit[a^c, Association @@ (# -> c b[#] & /@ Keys[b]), s];
unit /: (f : 
     Greater | Less | GreaterEqual | LessEqual | Equal | Unequal) [
   unit[a_, b_Association, ___], unit[c_, b_, ___]] := f[a, c];

   
GeV = unit[1, <|"GeV" -> 1|>];
m = unit[1, <|"m" -> 1|>]; 
s = unit[1, <|"s" -> 1|>]; 
kg = unit[1, <|"kg" -> 1|>]; 
kelvin = unit[1, <|"kelvin" -> 1|>]; 

c = 299792458 m/s;
hbar = 1.0545718 10^-34 m^2 kg/s;
hbarc = 1.97326979 10^-16 m GeV;
kB = 8.61733034 10^-14 GeV/kelvin;
G = 6.67408 10^-11 m^3/kg /s^2;

unity["m"] = {1/coeff[hbarc], -1};
unity["s"] = {1 / coeff[hbarc / c], -1};
unity["kg"] = {coeff[hbarc c / hbar], 1};
unity["kelvin"] = {coeff[kB], 1};

EndPackage[]