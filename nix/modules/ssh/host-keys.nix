# Fleet ed25519 host keys. sops-nix derives each host's age identity from this
# same key, so a replacement is checked by confirming `ssh-to-age` turns it
# into that host's recipient in .sops.yaml.
{
  atlas = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIq6iZ6dkKISb/PFpkSqNhwWTBCXI+Pr0MStILXbo6nG";
  hecate = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJpL68fdoen/7ejcXiWDCDAHFTEf7p24/H0nLa3q0U/3";
  janus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII9A+fHVGRpFJJZqMRlp4McKEaKbS5Cq7jYB+nwBS3Y9";
  pallas = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEsxHyOhquFTE5a39KFBpxEIDM1aitdunOT70CqHoqXH";
  prometheus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBTIWsSjCJNuf0KcKB2LzuvVx2DPY7HjxDwqP1Cwv6I7";
  vulcan = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIxqfiVtzbwTehEsXuYFXBAIUkMhQtKG4q4apRm4Fite";
}
