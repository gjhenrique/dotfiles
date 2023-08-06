module "work" {
  source  = "../tf_module"
  iso_url = "https://cloud-images.ubuntu.com/jammy/current/jammy-server-cloudimg-amd64.img"
  ssh_key = "/home/guilherme/.ssh/id_ed25519"

  domain = {
    hostname = "work"
    ip       = "192.168.122.10"
    vcpu     = 8
    memory   = 16384
    cloudinit_config = templatefile("${path.module}/cloud_init.cfg", {
      hostname   = "work"
      public_key = "/home/guilherme/.ssh/id_ed25519.pub"
      nix_file   = file("${path.module}/home.nix")
    })
    shared_folders = [{
      source   = "/opt/work"
      target   = "/opt/work"
      readonly = false
    }]
  }
}
