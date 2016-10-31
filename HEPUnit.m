BeginPackage["HEPUnit`"]

unit::differentUnit = "Units are different";
unity = <|"GeV" -> {1, 1}|>;
coeff[unit[a_, _Association, ___]] := a;
unitTable[unit[_, b_Association, ___]] := b;

unit[a_, <||>, ___] := a;

hasSameUnit[unit[_, b_Association, ___]] := b == unitTable[#] &;

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

convert[from_unit, to_unit] := Block[{
    f = toNaturalUnit[from],
    t = toNaturalUnit[to],
    cf := f[[1]],
    ef := f[[2]],
    ct := t[[1]],
    et := t[[2]]
    },
   cf ct^(-(ef/et)) to^(ef/et)
   ];

unit /: (f : Plus | Minus | Min | Max) [
  unit[a_, b_Association, s___], unit[c_, b_, ___]] := unit[f[a, c], b, s];

unit /: (Plus | Minus | Min | Max) [_unit, _unit] :=
  (Message[unit::differentUnit]; $Failed);

unit /: Times[unit[a_, b_Association, s___], 
   unit[c_, d_Association, ___]] := Block[{
    keySet = Union[Keys[b], Keys[d]],
    assocList := # -> Lookup[b, #, 0] + Lookup[d, #, 0] & /@ keySet,
    assoc := Association @@ (assocList /. (_ -> 0) -> Sequence[])
    },
   unit[a c, assoc, s]
   ];

unit /: Times[unit[a_, b_Association, s___], c_] := unit[a c, b, s];
unit /: Times[c_, unit[a_, b_Association, s___]] := unit[c a, b, s];

unit /: Power[_unit, 0] := 1;
unit /: Power[unit[a_, b_Association, s___], c_] := 
  unit[a^c, Association @@ (# -> c b[#] & /@ Keys[b]), s];
unit /: (f : 
     Greater | Less | GreaterEqual | LessEqual | Equal | Unequal) [
   unit[a_, b_Association, ___], unit[c_, b_, ___]] := f[a, c];

   
GeV = unit[1, <|"GeV" -> 1|>];
meter = unit[1, <|"meter" -> 1|>]; 
second = unit[1, <|"second" -> 1|>]; 
kg = unit[1, <|"kg" -> 1|>]; 
kelvin = unit[1, <|"kelvin" -> 1|>]; 

GeVQ = hasSameUnit[GeV];
mQ = hasSameUnit[meter];
sQ = hasSameUnit[second];
kgQ = hasSameUnit[kg];
kelvinQ = hasSameUnit[kelvin];

c0 = 299792458 meter / second;
hbar = 1.0545718 10^-34 meter^2 kg / second;
hbarc = 1.97326979 10^-16 meter GeV;
kB = 8.61733034 10^-14 GeV / kelvin;
G = 6.67408 10^-11 meter^3 / kg / second^2;

unity["meter"] = {1/coeff[hbarc], -1};
unity["second"] = {1 / coeff[hbarc / c0], -1};
unity["kg"] = {coeff[hbarc c0 / hbar], 1};
unity["kelvin"] = {coeff[kB], 1};

EndPackage[]
