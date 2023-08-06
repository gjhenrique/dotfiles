terraform {
  required_version = ">= 0.13"
  required_providers {
    libvirt = {
      source  = "dmacvicar/libvirt"
      version = "0.6.14"
    }
  }
}

provider "libvirt" {
  uri = "qemu:///system"
}

resource "libvirt_volume" "iso-qcow2" {
  name   = "qcow2-${var.domain.hostname}.iso"
  pool   = "default"
  format = "qcow2"
  source = var.iso_url
}

resource "libvirt_volume" "disk_resized" {
  name           = "disk-${var.domain.hostname}.iso"
  base_volume_id = libvirt_volume.iso-qcow2.id
  pool           = "default"
  size           = 100 * 1024 * 1024 * 1024 # 100 GB
}

data "template_file" "network_config" {
  template = file("${path.module}/network_config.cfg")

  vars = {
    ip = var.domain.ip
  }
}

resource "libvirt_cloudinit_disk" "cloudinit" {
  name           = "cloudinit-${var.domain.hostname}.iso"
  user_data      = var.domain.cloudinit_config
  network_config = data.template_file.network_config.rendered
  pool           = "default"
}

resource "libvirt_domain" "domain" {
  name   = var.domain.hostname
  memory = var.domain.memory
  vcpu   = var.domain.vcpu

  cloudinit = libvirt_cloudinit_disk.cloudinit.id

  network_interface {
    network_name = "default"
  }

  console {
    type        = "pty"
    target_port = "0"
    target_type = "serial"
  }

  console {
    type        = "pty"
    target_type = "virtio"
    target_port = "1"
  }

  disk {
    volume_id = libvirt_volume.disk_resized.id
  }

  graphics {
    type        = "spice"
    listen_type = "address"
    autoport    = true
  }

  dynamic "filesystem" {
    for_each = var.domain.shared_folders

    content {
      source   = filesystem.value["source"]
      target   = filesystem.value["target"]
      readonly = filesystem.value["readonly"]
    }
  }

  provisioner "remote-exec" {
    connection {
      host        = var.domain.ip
      type        = "ssh"
      user        = "henrique"
      private_key = var.ssh_key != null ? file(var.ssh_key) : file(pathexpand("~/.ssh/id_ed25519"))
    }

    # Copied from kubitect
    inline = [
      "while ! sudo grep \"Cloud-init .* finished\" /var/log/cloud-init.log; do echo \"Waiting for cloud-init to finish...\"; sleep 2; done"
    ]
  }
}
