{pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices."luks-5710ad9f-67e9-4959-b39c-2a671716bef3".device = "/dev/disk/by-uuid/5710ad9f-67e9-4959-b39c-2a671716bef3";
  #networking.hostName = "desktop"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # add mitmproxy
  security.pki.certificates = [
    ''
      -----BEGIN CERTIFICATE-----
      MIIDNTCCAh2gAwIBAgIUC3bo8aO85xWBMSRFA1yC2lpPcRswDQYJKoZIhvcNAQEL
      BQAwKDESMBAGA1UEAwwJbWl0bXByb3h5MRIwEAYDVQQKDAltaXRtcHJveHkwHhcN
      MjQwNzI2MTE0MDUzWhcNMzQwNzI2MTE0MDUzWjAoMRIwEAYDVQQDDAltaXRtcHJv
      eHkxEjAQBgNVBAoMCW1pdG1wcm94eTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCC
      AQoCggEBALTYD9qUNRqiM9GCBurFId4bFRFEf9IoFjr5AnuWTzOOtkR+O8ZW5ojr
      vnNeGzH2aoKJBdiPWqFNu1ohnVgomcyP/I139YfKnUtph0g5UyDknszwnn1vOlyl
      4wieofnU/5gP7kVyqewpwipGoGIVD9yt9mY3I4KaaM603cgZR0j+0RQOfXQp8uUA
      /tEOL+f2ww6//mtBOnK9hiWsTkxLt0N/Xfkhs2Miq7lEQPkNtNoVXxluA1smdrQE
      JtgBs9lRa1DUx4PieP0C1fhWewLWoEnX/VBimGvUw+eMJwstNUUxZ/zGhseyXqO2
      5+GEghX1iibAKtViPLvoQnNEv6ix218CAwEAAaNXMFUwDwYDVR0TAQH/BAUwAwEB
      /zATBgNVHSUEDDAKBggrBgEFBQcDATAOBgNVHQ8BAf8EBAMCAQYwHQYDVR0OBBYE
      FK4erNBihSNTVL0yHCNr0O0UWct8MA0GCSqGSIb3DQEBCwUAA4IBAQCXWUPoO/ZH
      vXXvIVpZuIriNkDTqcdgmnC7hX1TtSptgUWa4C1yUtbJ5ZvmchPLDb1/1dUXaxbK
      1BreaSzXeUzOkofX+zKnwdU7JZAdnlhBawxzpKTiHOQ/rRbXHXtSepMryqrUTYfO
      ki2OFpuu4vqSNGKwuBAJWUaogdWUjynYfDQMz/z/LUvBWwPRX4bsfbCjbrLMmo+Z
      Sjj9gDSi1QayMbg5bTlUwLSqbBwz+i0OTlBnrkIKBo4MXPFBFGPP3N4i5CRWdGbP
      DIQyR8x42CGTxf6h+6kzbrfPnFbuetDnaoHUG10XY7EHxA/xq9gVXhrNAd8Ps1pb
      KMzZvV9a8+ZS
      -----END CERTIFICATE-----
    ''
  ];

  services.jellyfin.enable = true;
  environment.systemPackages = [
    pkgs.jellyfin
    pkgs.jellyfin-web
    pkgs.jellyfin-ffmpeg
  ];
}
