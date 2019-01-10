Timely Advance
==============



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






