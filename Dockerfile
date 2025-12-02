FROM nixos/nix

# Install Emacs
RUN nix-env -iA nixpkgs.emacs

# Install Node.JS
RUN nix-env -iA nixpkgs.nodejs

# Move the whole project in.
WORKDIR /cli
COPY . .

# Install package dependencies.
WORKDIR /cli
RUN npm install --include=dev

# Expose it.
ENV PATH="${PATH}:/cli/bin"
