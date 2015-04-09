# OPAM version to install
export OPAM_VERSION=1.2.0

# OPAM packages needed to build tests
export OPAM_PACKAGES='ocamlfind menhir'

# Install OCaml and OPAM from apt
echo "yes" | sudo add-apt-repository ppa:avsm/ocaml42+opam12
sudo apt-get update -qq
sudo apt-get install -qq ocaml opam libc6-dev-i386

# Setup OPAM
echo "yes" | opam init
eval `opam config env`

# Install packages from OPAM
opam install -q -y ${OPAM_PACKAGES}

# Compile project
make
