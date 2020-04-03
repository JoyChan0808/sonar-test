_GIT_LAST_COMMIT_TIME=$(shell TZ=UTC git log --pretty=format:'%cd' -1 --date=format-local:'%Y%m%d-%H%M%S')
_GIT_LAST_COMMIT_HASH=$(shell git rev-parse --short HEAD)

_VERSION=$(_GIT_LAST_COMMIT_TIME).$(_GIT_LAST_COMMIT_HASH)

_SERVICE_NAME=playlist-service

_APP_DIRECTOR=playlist-service-app
_PROD_REGISTRY=886366864302.dkr.ecr.eu-west-1.amazonaws.com
_DEV_REGISTRY=office-hub.docker.aamcn.com.cn:5000

_PROD_IMAGE=$(_PROD_REGISTRY)/artsalliancemedia/playlist-service
_DEV_IMAGE=$(_DEV_REGISTRY)/artsalliancemedia/playlist-service

_CODACY_PATH=/home/ec2-user
_CODACY_APP=$(_CODACY_PATH)/codacy-coverage-reporter
_CODACY_VERSION=6.0.2
export CODACY_PROJECT_TOKEN=5810530976df4a1483809475548de8cb

version:
	@echo $(_VERSION)

mvn-build:
	mvn -D maven.test.skip=true package # TODO: unit test

build-prod: mvn-build
	docker build \
		--build-arg JAR_FILE=$(_APP_DIRECTOR)/target/$(_APP_DIRECTOR)-1.0-SNAPSHOT.jar \
		--build-arg REGISTRY=$(_PROD_REGISTRY) \
		-f $(_APP_DIRECTOR)/Dockerfile \
		-t $(_PROD_IMAGE):$(_VERSION) \
		.
	docker push $(_PROD_IMAGE):$(_VERSION)

build-dev: mvn-build
	docker build \
		--build-arg JAR_FILE=$(_APP_DIRECTOR)/target/$(_APP_DIRECTOR)-1.0-SNAPSHOT.jar \
		--build-arg REGISTRY=$(_DEV_REGISTRY) \
		-f $(_APP_DIRECTOR)/Dockerfile \
		-t $(_DEV_IMAGE):$(_VERSION) \
		.
	docker push $(_DEV_IMAGE):$(_VERSION)

render-dev-k8s-config:
	python deploy/render.py -t deploy/template.yml -e deploy/dev.env -n $(_SERVICE_NAME) \
		-i $(_DEV_IMAGE):$(_VERSION) -v $(_VERSION)

render-test-k8s-config:
	python deploy/render.py -t deploy/template.yml -e deploy/test.env -n $(_SERVICE_NAME) \
		-i $(_DEV_IMAGE):$(_VERSION) -v $(_VERSION)

codacy_report:
	if [ ! -f ${_CODACY_APP} ];then mkdir -p ${_CODACY_PATH};curl -Ls -o ${_CODACY_APP} "https://dl.bintray.com/codacy/Binaries/${_CODACY_VERSION}/codacy-coverage-reporter-linux";fi
	mvn test
	chmod u+x ${_CODACY_APP}
	${_CODACY_APP} report -l Java -r playlist-service-app/target/site/jacoco/jacoco.xml
