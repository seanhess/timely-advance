Nimble Finance
==============

Design Goals
------------

* Monolithic, modular repository
* Avoid services, but stricly separate modules so it can be moved later
* Mobile applications are thin wrappers around the web.



Workflows
---------

Make a whole "fake experience" to connect all the systems
* Account: Email, Phone -> Text
* Bank: use Plaid
* Bank service: use Plaid
* Approval: always approved
* Evaluate: If their balance is less than $X send them money



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






