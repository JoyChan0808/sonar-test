#!/usr/bin/env groovy

def user = 'artsalliancemedia'
def repo = 'playlist-service'
def registry = '886366864302.dkr.ecr.eu-west-1.amazonaws.com'

node('aam-identity-prodcd') {
    properties([
        [
            $class: 'GithubProjectProperty',
            displayName: '',
            projectUrlStr: "https://github.com/${user}/${repo}/"
        ],
    ])

    stage('Checkout') {
        checkout scm
    }

    if (env.BRANCH_NAME == 'master' || env.BRANCH_NAME == 'staging') {
        def imageTag = getTag()
        def serviceImage = "${user}/${repo}"
        def environment = getEnvironment(env.BRANCH_NAME)

        stage('Init') {
            dir("infrastructure/${environment}") {
                withEnv([
                   'AWS_DEFAULT_REGION=eu-west-1',
                   "TF_VAR_release=${imageTag}",
                   "TF_VAR_service_image=${serviceImage}:${imageTag}"
                ]){
                    sh 'terraform init'
                    sh "terraform plan -no-color"
                  }
            }

        }

        stage('Test') {
            echo "skip test now"
        }

        stage('Build') {
            sh 'make build-prod'
        }

        stage('Deploy') {
            withEnv([
                "TF_VAR_service_image=${serviceImage}:${imageTag}",
                "TF_VAR_release=${imageTag}",
                'AWS_DEFAULT_REGION=eu-west-1'
            ]) {
                dir("infrastructure/${environment}") {
                    sh 'terraform init'
                    sh 'terraform plan -no-color -out=plan'
                    sh 'terraform apply -auto-approve=true plan'
                    }
                }
            }

    }
    stage('Sonar') {
           sh 'mvn clean package'
           sh 'mvn sonar:sonar -Dsonar.host.url=http://office.aamcn.com.cn:4015 -Dsonar.login=837e19cd3b6e3a8e0ef3177e035caaef55b6509f'
    }


}

def getTag() {
    def tag = sh returnStdout: true, script: 'make version'
    return tag.trim()
}

def getEnvironment(branch) {
    if (branch == 'staging') return 'staging'
    else if (branch == 'master') return 'production'
    else error 'Unknown deployment environment'
}
