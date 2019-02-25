build: build-server build-web

build-server:
	cd server && stack image container --docker
	docker tag timely:latest registry.gitlab.com/timely-advance/timely:$(version)

build-web:
	cd web && docker build -t registry.gitlab.com/timely-advance/timely/web:$(version) .

push:
	docker push registry.gitlab.com/timely-advance/timely:$(version)
	docker push registry.gitlab.com/timely-advance/timely/web:$(version)



