final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hfinal: hprev: {
      n2t-hdl = hfinal.callCabal2nix "n2t-hdl" (./n2t-hdl) { };
      });
  });
}
