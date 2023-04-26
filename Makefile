irpn: build/exec/irpn.js

build/exec/irpn.js: irpn.ipkg pack.toml src/*.idr img_id
	docker cp "$$(cat img_id):/root/build/" .

img_id:
	docker build . -t irpn
	docker create irpn > img_id

base-image: base-image.docker irpn.ipkg pack.toml
	docker build -f base-image.docker -t irpn
	docker tag irpn-base:latest ghcr.io/emdash/irpn-base:latest
	docker push ghcr.io/emdash/irpn-base:latest


clean:
	rm -r build/
	docker rm "$$(cat img_id)"
	docker rmi irpn
	rm img_id

.PHONY : base-image clean
