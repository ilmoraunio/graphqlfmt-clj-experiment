SOURCE = $(wildcard src/*/* resources/*)

target/graphqlfmt.jar: $(SOURCE)
	lein uberjar
	mv target/graphqlfmt-0.1.0-SNAPSHOT-standalone.jar $@

# Linux

.PHONY: graalvm-native-image-linux
graalvm-native-image-linux: Dockerfile.linux
	docker build -t $@ -f $< .

.PHONY: build-linux
build-linux: graalvm-native-image-linux target/graphqlfmt.jar 
	./compile-linux $<

# MacOS

graalvm-darwin-amd64.tar.gz:
	wget -O graalvm-darwin-amd64.tar.gz https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-22.3.0/graalvm-ce-java19-darwin-amd64-22.3.0.tar.gz

.graalvm-ce-darwin-amd64: graalvm-darwin-amd64.tar.gz
	mkdir $@
	tar --directory $@ -xvf $< --strip-components 1

.graalvm-ce-darwin-amd64/Contents/Home/bin/native-image: .graalvm-ce-darwin-amd64
	./$</Contents/Home/bin/gu install native-image

.PHONY: build-darwin-amd64
build-darwin-amd64: target/graphqlfmt.jar .graalvm-ce-darwin-amd64/Contents/Home/bin/native-image
	./compile-darwin

