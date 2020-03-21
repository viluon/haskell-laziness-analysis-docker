FROM archlinux:20200205
RUN pacman -Syu --noconfirm
RUN pacman -S --noconfirm git base-devel
RUN pacman -S --noconfirm go

RUN useradd --create-home packager
# use all possible cores for subsequent package builds
RUN sed -i 's,#MAKEFLAGS="-j2",MAKEFLAGS="-j$(nproc)",g' /etc/makepkg.conf
# don't compress the packages built here
RUN sed -i "s,PKGEXT='.pkg.tar.xz',PKGEXT='.pkg.tar',g" /etc/makepkg.conf
RUN echo '' >> /etc/sudoers
RUN echo 'packager      ALL = NOPASSWD: /usr/sbin/makepkg *' >> /etc/sudoers
RUN echo 'packager      ALL = NOPASSWD: /usr/sbin/pacman *'  >> /etc/sudoers

USER packager
WORKDIR /home/packager/
RUN git clone https://aur.archlinux.org/yay.git
WORKDIR /home/packager/yay/
RUN makepkg -i --noconfirm

WORKDIR /home/packager/
RUN yay -S --noconfirm ghcup-git exa
COPY bashrc.sh .
RUN mv ~/bashrc.sh ~/.bashrc

RUN ghcup install 8.6.5
RUN ghcup set 8.6.5
RUN ghcup install-cabal

RUN git clone --recursive --depth 1 -b ghc-8.8.3-release https://gitlab.haskell.org/ghc/ghc
RUN yay -S --noconfirm python python-sphinx libedit numactl
RUN yay -S --noconfirm stack-bin

USER root
RUN ln -s /home/packager/.ghcup/bin/ghc     /usr/sbin/ghc
RUN ln -s /home/packager/.ghcup/bin/ghc-pkg /usr/sbin/ghc-pkg
RUN stack install --system-ghc alex
RUN stack install --system-ghc happy
RUN ln -s /root/.local/bin/alex             /usr/sbin/alex
RUN ln -s /root/.local/bin/happy            /usr/sbin/happy
RUN chmod a+x  /root/
RUN chmod a+x  /root/.local/
RUN chmod a+x  /root/.local/bin/
RUN chmod a+rx /root/.local/bin/alex
RUN chmod a+rx /root/.local/bin/happy

USER packager
WORKDIR /home/packager/ghc/
RUN ./boot
RUN ./configure
RUN /home/packager/.ghcup/bin/cabal update
RUN bash -c 'source ~/.bashrc; ./hadrian/build.sh -j'
