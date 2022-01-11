SOURCE = $(wildcard src/*/* resources/*)

target/graphql-fmt.jar: $(SOURCE)
	lein uberjar
	mv target/graphql-fmt-0.1.0-SNAPSHOT-standalone.jar $@

# Linux

.PHONY: graalvm-native-image-linux
graalvm-native-image-linux: Dockerfile.linux
	docker build -t $@ -f $< .

.PHONY: build-linux
build-linux: graalvm-native-image-linux target/graphql-fmt.jar 
	./compile-linux $<

# MacOS

graalvm-darwin.tar.gz:
	wget -O graalvm-darwin.tar.gz https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-19.3.0/graalvm-ce-java8-darwin-amd64-19.3.0.tar.gz

.graalvm-ce-java8-darwin: graalvm-darwin.tar.gz
	mkdir $@
	tar --directory $@ -xvf $< --strip-components 1

.graalvm-ce-java8-darwin/Contents/Home/bin/native-image: .graalvm-ce-java8-darwin
	./$</Contents/Home/bin/gu install native-image

.PHONY: build-darwin
build-darwin: target/graphql-fmt.jar .graalvm-ce-java8-darwin/Contents/Home/bin/native-image
	./compile-darwin

