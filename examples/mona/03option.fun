type Option[a] = None | Some[a];
just x = Some x;
maybe n o = case o of { None -> n; Some x -> x };
main = maybe 0 (just 42);
