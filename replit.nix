{ pkgs }: {
    deps = [
        pkgs.sudo
        (pkgs.haskellPackages.ghcWithPackages (pkgs: [
            # Put your dependencies here!
        ]))
        pkgs.haskell-language-server
    ];
}