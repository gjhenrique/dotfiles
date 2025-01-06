{pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  services.tlp.enable = true;

  security.pki.certificates = [
    ''
      -----BEGIN CERTIFICATE-----
      MIIDNTCCAh2gAwIBAgIUPyrQfQcN1lO9VfFVxWvwfhzqqsgwDQYJKoZIhvcNAQEL
      BQAwKDESMBAGA1UEAwwJbWl0bXByb3h5MRIwEAYDVQQKDAltaXRtcHJveHkwHhcN
      MjQxMDExMTgyODU0WhcNMzQxMDExMTgyODU0WjAoMRIwEAYDVQQDDAltaXRtcHJv
      eHkxEjAQBgNVBAoMCW1pdG1wcm94eTCCASIwDQYJKoZIhvcNAQEBBQADggEPADCC
      AQoCggEBAL3GLPdLaroS8DNw+wzHxuEUVcUBPDfV7c7aQQbeayGtaODaAy2npTXk
      fiS4YsYEADHOaySrvB32V1nBjgeuY1wKZXKm2LPFR2n4Q+buk/7Q74llZcuzWFOS
      6ywtWb0vWCHH+sbY0PpFBz/VfEi/AQcXLmKx6pxAQ+K+dSCfaf17hXJSXx2v1tZw
      u3B6kj1ULFrPRWWGEd6oJKw5FzHnWS+Ki6MyFjAI1QU6xgahJp3+h3zKRARNbm7f
      h2HDULymV4R8xMoh9jhjBkJMIGDxZ1FsQda4tFq9eBwWn09tbVt5WyyBvcTz+r46
      Q4H9/zm0h1/K1swJwfMqTOtnqwwt6PECAwEAAaNXMFUwDwYDVR0TAQH/BAUwAwEB
      /zATBgNVHSUEDDAKBggrBgEFBQcDATAOBgNVHQ8BAf8EBAMCAQYwHQYDVR0OBBYE
      FE03oiw93q5+6Q5Etn9GLqdHOef1MA0GCSqGSIb3DQEBCwUAA4IBAQClvJX+qtaf
      27qFmhxnhspRWOFHuNAA3mVIeCO8UPanfZorksntZ8+4F+MqsbETW0K41ck10Wpo
      CHABYkLPKMM8rUYdsMo7HIDjdDk3ks0rnwmriPtAdlzoe2Kkke51ZPfMCL+eKCRi
      hzcvYcaPQB4JJUUAe5mbveSkLTh5vv2nKCZVg5ErtoUvXoe0R+QrEchT6GnSZIMX
      3G5azwJwIg+q7vuifhl7rmTfWRxX6rm7EO1p/Aqvd4l9dkCJnCvBLisT1rJoIEsx
      6RxDaOTTcr5rterDw4LFNhREG0HWxMbeAPHpthhl1N2T7O/XoA0CatowzEN6N9aH
      whM4k6iGYQpb
      -----END CERTIFICATE-----
    ''
  ];
}
