resource "random_pet" "tapu" {}

resource "random_pet" "gadsden" {}

resource "random_pet" "rolo" {}

resource "random_password" "letters" {
  length  = 10
  lower   = true
  upper   = true
  number  = false
  special = false
}

resource "random_password" "numbers" {
  length  = 15
  lower   = false
  upper   = false
  number  = true
  special = false
}
