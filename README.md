Timely Advance
==============



Integration Tests
-----------------

   $ stack ghci
   > Api.start' runAppTest
   > AccountOnboard.start' runAppTest

Then run them from admin


Server
------


Ok, if I use an nginx-ingress-controller, the platform neutral version, it'll port across to a different host. It'll create an EBL instance. That should work. I can follow a generic guide, I think

* https://medium.com/devopslinks/learning-kubernetes-on-eks-by-doing-part-4-ingress-on-eks-6c5e5a34920b
* https://gist.github.com/camilb/69008e4dddeb5d663e2e6699b3a93283 - example
* https://www.sumologic.com/blog/amazon-web-services/aws-elb-alb/ - but ALBs are better and more flexible. Should I even configure one with k8? Not really, because it's not platform neutral. What if I set it up manually? It's not something I need to change often. And a web interface sounds nice. Let's get an echo server going


AWS EKS Setup
-------------
https://docs.aws.amazon.com/eks/latest/userguide/getting-started.html

You must log in as the IAM user you will use to authenticate before creating it with the console. Create the user, give it permissions, then log in with it by setting a password.

Download iam, aws, etc.

    aws --version
    aws-iam-authenticator help
    aws configure

Save configuration

    aws eks update-kubeconfig --name timely

Set up the Dashboard - https://docs.aws.amazon.com/eks/latest/userguide/dashboard-tutorial.html

Set up Ingress - https://github.com/kubernetes-sigs/aws-alb-ingress-controller

SSL Redirect - https://kubernetes-sigs.github.io/aws-alb-ingress-controller/guide/tasks/ssl_redirect/

See deploy/aws - modify the access key and secret to create the ingress deployment

Create default storage class - https://docs.aws.amazon.com/eks/latest/userguide/storage-classes.html

    kube apply -f deploy/aws/storage-class.yaml




Kubernetes Cluster from Scratch
-------------------------------

Create the cluster. Save the config to kube config and test the connection

    kubectl get nodes

Install [Kubernetes Dashboard](https://github.com/kubernetes/dashboard)

    kubectl apply -f https://raw.githubusercontent.com/kubernetes/dashboard/v1.10.1/src/deploy/recommended/kubernetes-dashboard.yaml
    kubectl proxy

Now you can [visit the Dashboard](http://localhost:8001/api/v1/namespaces/kube-system/services/https:kubernetes-dashboard:/proxy/#!/overview)

Create an [admin user](https://github.com/kubernetes/dashboard/wiki/Creating-sample-user)

    kubectl create -f deploy/init/admin.yaml

Get the secret from the admin-user and use it to log in to the dashboard

    kubectl -n kube-system describe secret $(kubectl -n kube-system get secret | grep admin-user | awk '{print $1}')


Deploy Application
------------------

Create a gitlab deploy token: https://docs.gitlab.com/ee/user/project/deploy_tokens/. Replace the user and password
 
    kubectl create secret docker-registry gitlab-registry --docker-server=registry.gitlab.com --docker-username=XXXX --docker-password=XXXX --docker-email=sean.hess@timelyadvance.com



Kubernetes Cheat Sheet
----------------------


Access a service via port forwarding

    kubectl port-forward svc/echo1 8082:80

SSH into a running pod

    kubectl exec -it shell-demo -- /bin/bash





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




### Digital Ocean Tutorials

    kubectl get services
    kubectl get pv
    kubectl describe service sample-load-balancer




Docker
------
Gitlab private registry: https://gitlab.com/timely-advance/timely/container_registry

    > docker tag timely:latest registry.gitlab.com/timely-advance/timely



Todo
----

- [ ] CRUD app
- [ ] Plaid Bank Integration
- [ ] Bank Service
- [ ] Fake authentication - enter phone number, fake code, etc
- [ ] Fake Experience
- [ ] Two-factor authentication
- [ ] iOS app (after full fake experience)


Design Goals
------------

* Monolithic, modular repository
* Avoid services, but stricly separate modules so it can be moved later
* Mobile applications are thin wrappers around the web.


Workflows
---------

CRUD App
* Create Account: Email, First Name, Last Name
* List of all accounts in the system
* Details

Plaid Bank Integration
* Uses plaid to fetch bank details
* Get bank and transaction information

Make a whole "fake experience" to connect all the systems
* Account: Email, Phone -> Text
* Bank: use Plaid
* Bank service: use Plaid
* Approval: always approved
* Evaluate: If their balance is less than $X send them money

iOS app
* How does navigation work?
* Save authenticaiton token and pass to the app


System Diagram
--------------

* These each store all of the info from their process, so nothing is lost, they return RELEVANT info, and you can look up history later
* Should they fetch the data they need? YES, there is a master funciton/job that collects info, and calls the logic. Remember calling things may depend on previous results too! But only upstream information. They should be passed the account, since they don't own it. I guess the other possible design is that they go fetch account too, which... it makes sense. Ok, the master point could fetch the current account info, that's fine.
* Should they be asynchronous jobs? I think apply and evaluate should be async jobs. The services should be call/response
* Each module has its own separate, duplicated storage. Doesn't mean they're the central authority though






### Apply
(Stateless, action)
Input: consumer information, bank history
Process: underwriting
Output: approval, account metadata

### Evaluate
(Stateless, action)
Input: bank history, account info
Output: nothing, send money, take money

### Transfer
(Stateless, action)
send or pull money from someone, store everything

### Accounts
(Data) active account information, history, etc. Change settings, see everything in one place. Store everything.





    +---------+   +---------+   +---------+
    |         |   |         |   |         |   * All web based for MVP
    |   iOS   |   | Android |   |   Web   |
    |         |   |         |   |         |
    |         |   |         |   |         |
    +---------+   +---------+   +---------+
         ^             ^             ^
         |             |             |
         v             v             v
    +------------------------------------------------------------------+   
    |                        Application Server                        |                         
    +------------------------------------------------------------------+                         


      +----------------------+
      |                      |
      |       Accounts       |
      |                      |
      +----------------------+


      +--------------------------------------------------------------------------------------+
      |                              Events                                                  |
      +--------------------------------------------------------------------------------------+
                                                                                                     
      +----------------------+      +----------------------+     +----------------------+ 
      |                      |      |                      |     |                      |                      
      |       Apply          |      |       Evaluate       |     |       Transfer       |                      
      |                      |      |                      |     |                      |                      
      +---+--+---------------+      +---------+------------+     +----------+-----------+ 
          |  |                                |                             |                           
          |  |                                |                      +------+---------+                 
          |  |                                |                      |      ACH       |                 
          |  |                                |                      +----------------+                                         
          |  +----------------------------+   |                               
          |                               |   |                               
        +-+------------------------+   +--+---+------------+                  
        | Consumer Info (Experian) |   |  Bank Info (MX)   |                  
        +--------------------------+   +-------------------+                  






