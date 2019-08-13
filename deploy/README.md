Deploy
======



Useful Commands
---------------

Logs for a given service

    $ kube logs -f svc/api

Access a service via port forwarding

    $ kubectl port-forward svc/postgres 5432:5432

Get a list of services and pods

    $ kube get svc
    $ kube get pod

SSH into a running pod

    $ kubectl exec -it shell-demo -- /bin/bash

[Kubernetes Dashboard](http://localhost:8001/api/v1/namespaces/kube-system/services/https:kubernetes-dashboard:/proxy/#!/ingress?namespace=default)

    # get the key
    $ kubectl -n kube-system describe secret $(kubectl -n kube-system get secret | grep eks-admin | awk '{print $1}')
    
    # proxy the application
    $ kube proxy


Deploy
-------

    $ deploy/build 0.10.675dd936

    Deploy VERSION=0.10.675dd936
    Build deploy/builds/0.10.675dd936
    - api.yaml
    - schedule-advance-collect.yaml
    - static.yaml
    - work-account-onboard.yaml
    - work-account-update.yaml
    - work-advance-collect.yaml
    - work-advance-send.yaml
    Built deploy/builds/0.10.675dd936
    kubectl apply -f deploy/builds/0.10.675dd936
    
    $ kubectl apply -f deploy/builds/0.10.675dd936

    service/api configured
    deployment.extensions/api unchanged
    cronjob.batch/schedule-advance-collect unchanged
    service/web-static configured
    deployment.apps/web-static unchanged
    deployment.extensions/work-account-onboard unchanged
    deployment.extensions/work-account-update unchanged
    deployment.extensions/work-advance-collect unchanged
    deployment.extensions/work-advance-send unchanged


AWS EKS Setup
-------------
- [Getting Starting with EKS](https://docs.aws.amazon.com/eks/latest/userguide/getting-started-eksctl.html)

Create a cluster and test

    $ eksctl create cluster --name timely-app
    $ eksctl get cluster timely-app
    NAME	VERSION	STATUS	CREATED			VPC		SUBNETS								SECURITYGROUPS
    timely	1.11	ACTIVE	2019-02-28T16:55:38Z	vpc-0b28339e4f082be81	subnet-0008206f56202428a,subnet-03022082739f3516f,subnet-0c7851c425e597d03	sg-00080f4652a47d18a

Save configuration to kubeconfig default

    aws eks update-kubeconfig --name timely-app

[Install Dashboard](https://docs.aws.amazon.com/eks/latest/userguide/dashboard-tutorial.html)

[ALB Ingress Controller](https://docs.aws.amazon.com/eks/latest/userguide/alb-ingress.html)


Get the public URL of the deployed ingress

    $ kubectl get ingress/timely-ingress
    NAME             HOSTS   ADDRESS                                                                  PORTS   AGE
    timely-ingress   *       1f3c3497-default-timelying-d6d4-1909489868.us-east-2.elb.amazonaws.com   80      11m

Debuggin the ALB controller

    $ kube get pod -n kube-system
    NAME                                      READY   STATUS    RESTARTS   AGE
    alb-ingress-controller-79d89fdbd7-kzv5b   1/1     Running   0          9s
    $ kube logs -f alb-ingress-controller-79d89fdbd7-kzv5b -n kube-system

    # you can get more information by uncommenting --aws-api-debug


DNS Configuration
-----------------

Get the public address of the ALB

    $ kubectl get ingress/timely-ingress
    NAME             HOSTS   ADDRESS                                                                  PORTS   AGE
    timely-ingress   *       1f3c3497-default-timelying-d6d4-1909489868.us-east-2.elb.amazonaws.com   80      11m

Assign a CNAME to that URL

    CNAME test.timelyadvance.com 1f3c3497-default-timelying-d6d4-1909489868.us-east-2.elb.amazonaws.com

> Create default storage class - https://docs.aws.amazon.com/eks/latest/userguide/storage-classes.html
>     kube apply -f deploy/aws/storage-class.yaml





Deploy Application
------------------

Create a gitlab deploy token: https://docs.gitlab.com/ee/user/project/deploy_tokens/. Replace the user and password
 
    kubectl create secret docker-registry gitlab-registry --docker-server=registry.gitlab.com --docker-username=XXXX --docker-password=XXXX --docker-email=sean.hess@timelyadvance.com







Gitlab Runner
-------------

* https://gitlab.com/groups/timely-advance/-/settings/ci_cd
* https://about.gitlab.com/2016/04/19/how-to-set-up-gitlab-runner-on-digitalocean/
* https://docs.gitlab.com/ee/ci/docker/using_docker_build.html

Install stack, docker, etc

    apt-get update -y
    apt-get install libpq-dev -y
    https://docs.haskellstack.org/en/stable/README/



K8
-------

Configure KUBECONFIG to look for kube.config in the current directory

    export KUBECONFIG=$KUBECONFIG:kube.config


- You don't need minikube. Docker comes with everything
- https://medium.com/slalom-technology/get-kubernetes-running-locally-on-osx-and-windows-b3b5f176b5bb
- https://github.com/kubernetes/dashboard
- kubectl proxy


https://kubernetes.io/docs/tasks/access-application-cluster/configure-access-multiple-clusters/ - You can specify the --kubeconfig flag to specify a specific file

    kubectl config --kubeconfig=config-demo use-context dev-frontend
    kubectl config --kubeconfig=config-demo view --minify

    export  KUBECONFIG=$KUBECONFIG:config-demo:config-demo-2

You could set $KUBECONFIG to be equal to something like the following, which would check for a local file and merge global

    export KUBECONFIG=$KUBECONFIG:kube.config:$HOME/.kube/config


https://kubernetes.io/docs/concepts/configuration/organize-cluster-access-kubeconfig/ - they merge config files using an environment variable.. weird. 


https://kubernetes.io/docs/concepts/configuration/overview/

* Store in a single file
* kubectl can be called on a directory of config files
* Pods should be bound to a ReplicaSet or Deployment, or they won't come back up. 

https://docs.gitlab.com/ee/user/project/clusters/



https://kubernetes.io/docs/concepts/overview/object-management-kubectl/overview/

Imperitive management - well, this is doable at least

    kubectl create -f cron.yaml
    kubectl delete -f cron.yaml
    kubectl replace -f cron.yaml

Declarative management

    kubectl create -f cron.yaml --save-config
    kubectl apply -f cron.yaml

https://kubernetes.io/docs/reference/kubectl/conventions/

* don't use :latest

 https://kubernetes.io/docs/reference/kubectl/cheatsheet/

     kubectl logs my-pod   

https://stackoverflow.com/questions/50069920/why-should-i-store-kubernetes-deployment-configuration-into-source-control-if-ku

* Rollback is for emergencies. Store config in repositories and manage that way.


https://kubernetes.io/docs/tutorials/stateless-application/expose-external-ip-address/
https://kubernetes.io/docs/concepts/services-networking/connect-applications-service/

* Expose with a service. 

   kubectl exec -it postgres-556644cdfc-6r9tm -- /bin/bash

 https://kubernetes.io/docs/tasks/configure-pod-container/pull-image-private-registry/

* Pull from a private registry


https://helm.sh/docs/using_helm/#using-ssl-between-helm-and-tiller

* install Helm and tiller using SSH

https://www.digitalocean.com/community/tutorials/how-to-set-up-an-nginx-ingress-with-cert-manager-on-digitalocean-kubernetes

* Install an ingress controller (nginx)
* Includes a load balancer
* Multiple domains
* SSL / TLS






Docker
------
Gitlab private registry: https://gitlab.com/timely-advance/timely/container_registry

    > docker tag timely:latest registry.gitlab.com/timely-advance/timely

