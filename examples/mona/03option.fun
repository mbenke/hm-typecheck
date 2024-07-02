type Option[a] = None | Some[a];
just x = Some x;
kmaybe n o = case o of { None -> n; Some x -> x };
main = kmaybe 0 (just 42);
