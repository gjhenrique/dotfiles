{
  pkgs,
  stable,
  system,
  ...
}: {
  home.packages = with pkgs; [
    #desktop specific
    grim
    pinta
    okular
    slurp
    swappy

    # tools
    bluetuith
    foot
    emacs29-pgtk
    # for compiling tree-sitter
    libgcc

    # cli
    nftables
    nodePackages.js-beautify
    playerctl
  ];

  services.mako = {
    enable = true;

    defaultTimeout = 5000;
    width = 300;
    height = 200;
    padding = "20";
    margin = "20";
    font = "JetBrainsMono NF 14";
    backgroundColor = "#24273a";
    borderColor = "#8aadf4";
    textColor = "#cad3f5";

    extraConfig = ''
      [urgency=high]
      border-color=#f5a97f
      [mode=do-not-disturb]
      invisible=1
    '';
  };

  services.kanshi = {
    enable = true;
    systemdTarget = "graphical-session.target";

    settings = [
      {
        profile.name = "desktop";
        profile.outputs = [
          {
            criteria = "LG Electronics LG HDR 4K 0x00067273";
            mode = "3840x2160";
            position = "0,0";
            scale = 2.0;
            transform = "270";
          }
          {
            criteria = "BNQ BenQ EW3270U G9K02925019";
            mode = "3840x2160";
            position = "1080,300";
            scale = 1.666667;
          }
          {
            criteria = "Dell Inc. DELL U2715H GH85D71G1R9S";
            mode = "2560x1440";
            position = "3384,30";
            scale = 1.33333333;
            transform = "90";
          }
        ];
      }
      {
        profile.name = "desktop-2";
        profile.outputs = [
          {
            criteria = "LG Electronics LG HDR 4K 0x00067273";
            mode = "3840x2160";
            position = "0,0";
            scale = 2.0;
            transform = "270";
          }
          {
            criteria = "BNQ BenQ EW3270U G9K02925019";
            mode = "3840x2160";
            position = "1080,300";
            scale = 1.666667;
          }
        ];
      }
      {
        profile.name = "docked-notebook";
        profile.outputs = [
          {
            criteria = "BNQ BenQ EW3270U G9K02925019";
            mode = "3840x2160";
            position = "0,0";
            scale = 2.0;
          }
          {
            criteria = "eDP-1";
            position = "1930,0";
            scale = 2.0;
          }
        ];
      }
    ];
  };

  programs.waybar = {
    enable = true;
    systemd.enable = true;

    style = builtins.readFile ./files/waybar-style.css;
    settings = [
      {
        position = "top";
        height = 30;
        modules-left = ["hyprland/workspaces"];
        modules-center = ["clock"];
        modules-right = ["cpu" "memory" "pulseaudio" "battery"];

        clock = {
          format = "󰸗 {:%d.%m - %H:%M}";
          interval = 1;
          tooltip-format = "<big>{:%B %Y}</big>\n<tt>{calendar}</tt>";
          on-click = "hyprctl dispatch exec xdg-open https://calendar.google.com";
        };

        cpu = {
          format = "{usage}% ";
          interval = 1;
        };

        memory = {
          format = "{percentage}% 󰍛";
          interval = 1;
        };

        pulseaudio = {
          format = "{icon} {volume}%";
          format-bluetooth = "{volume}% {icon} {format_source}";
          format-bluetooth-muted = "ﱝ {icon} {format_source}";
          format-muted = "ﱝ";
          format-source = "{volume}% ";
          format-source-muted = "";
          format-icons = {
            headphones = "";
            handsfree = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = ["" "" ""];
          };
          on-click = "pavucontrol";
        };

        battery = {
          states = {
            warning = 30;
            critical = 15;
          };
          format = "{capacity}% {icon}";
          format-charging = "{capacity}% 󰂅";
          format-icons = ["󰁹" "󰂂" "󰂁" "󰂀" "󰁿" "󰁾" "󰁽" "󰁼" "󰁻" "󰁺"];
          tooltip-format = "{time}";
        };
      }
    ];
  };

  xdg.portal = {
    enable = true;

    extraPortals = [pkgs.xdg-desktop-portal-hyprland];
  };

  wayland.windowManager.hyprland = {
    enable = true;

    package = stable.hyprland;
    extraConfig = builtins.readFile ./files/hyprland.conf;
  };

  systemd.user.services.polkit-gnome = {
    Unit = {
      Description = "polkit-gnome-authentication-agent-1";
      PartOf = "graphical-session.target";
      Requires = "graphical-session.target";
      After = "graphical-session.target";
    };

    Service = {
      ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
      Restart = "on-failure";
    };

    Install = {WantedBy = ["graphical-session.target"];};
  };

  programs.foot = {
    enable = true;
    server.enable = true;

    settings = {
      main = {
        font = "JetBrainsMono NF:size=12";
      };
    };
  };

  gtk = {
    enable = true;
    cursorTheme = {
      name = "phinger-cursors-dark";
    };
  };

  services.gpg-agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gtk2;
  };

  imports = [
    (import ./work-linux.nix {inherit pkgs;})
  ];
}
