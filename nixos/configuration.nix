# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.device = "/dev/sda";

  # networking.hostName = "nixos"; # Define your hostname.

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Paris";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    manpages firefox

    vim emacs25 gitAndTools.gitFull

    i3status feh rxvt_unicode urxvt_perl urxvt_perls urxvt_theme_switch urxvt_font_size 

    xlsfonts xfontsel

    vpnc

    dmenu xscreensaver xclip		

    (
      with import <nixpkgs> {};

      vim_configurable.customize {
        name = "vim";
        vimrcConfig.customRC = ''
          syntax enable
          expandtab  
          ts=2
        '';
      }
    )
  ];

  nixpkgs.config = {
    allowUnfree = true;
  };

  environment.sessionVariables = {
    EDITOR = "vim";
    NIXPKGS_ALLOW_UNFREE = "1";
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable the X11 windowing system.
  # Enable i3
  services.xserver = {
    enable = true;
    layout = "fr";
    xkbVariant = "bepo";
    windowManager.i3.enable = true;
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";
    displayManager = {
      slim = {
        enable = true;
        defaultUser = "morsicus";
      };
      sessionCommands = ''
        ${pkgs.feh}/bin/feh --no-fehbg --bg-tile ~/.config/i3/wallpaper.png &
      '';
    };
  };

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = false;
  };

  programs = {
    ssh.startAgent = true;
    bash.enableCompletion = true;
    zsh.enable = true;
  };

  # Enable redshift
  services.redshift = {
    enable = true;
    brightness.day = "0.8";
    brightness.night = "0.1";
    latitude = "0.0000";
    longitude = "0.0000";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.morsicus = {
    name = "morsicus";
    group = "users";
    extraGroups = [
      "wheel" "disk" "audio" "video"
      "networkmanager" "systemd-journal"
    ];
    createHome = true;
    home = "/home/morsicus";
    shell = "/run/current-system/sw/bin/zsh";
    isNormalUser = true;
    uid = 1000;
  };

  # Show manual on Virtual Console 8;
  services.nixosManual.showManual = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

}
