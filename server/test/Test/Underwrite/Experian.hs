{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Underwrite.Experian where


import Data.Aeson                             (Value (String), toJSON, object, (.=))
import Data.Model.Types                       (Address (..))
import Data.Model.Valid                       (Valid (..))
import Data.ByteString (ByteString)
import Data.String.Here
import Network.Experian.CreditProfile.Request (Dob (..))
import Test.Dates                             (day)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Monad
import Timely.Underwrite                      (Application (..))
import Timely.Underwrite.Experian             (Config (..), Credentials)
import Network.Experian.CrossCore as CrossCore (toString, signature, HMACSecret(..))


specExperian = do
  ts <- runTests tests
  defaultMain $ testGroup "tests" $ ts


tests :: Tests ()
tests = do
  group "applicant" $
    test "dob should serialize" $ do
      toJSON (Dob (day "1937-11-01")) @?= object ["dob" .= String "11011937"]


  group "hmac" testHMAC


testHMAC :: Tests ()
testHMAC = do
  let secret = HMACSecret "cfdb8f6a-d0d6-4836-a699-b4a3415b7186"
  let sign = CrossCore.toString . CrossCore.signature secret

  test "simple string" $
     sign "hello" @?= "iW/pCzHi7HlNbwjVwKFJHZcRAmTLjb5kiE6sNLbJZx8="

  test "with quotes" $
     sign "\"hello\"" @?= "5ycBOIYqRNwQphZEvpC1XgmEh/N5wGLzbteQLu0ogno="

  test "small object" $
     sign "{\"key\":\"value\"}" @?= "epNS//o6V6AB+fc06CSdVG8a8Y7lpalQiROTGPZm3+s="

  test "large object" $
     sign body @?= "apb208XCLTN7oc/9sjGksGg6IC62uABFNXoDu6cUK8Y="

  test "newlines" $
     sign bodyNewline @?= "41OKC9kTJhq/ZGZ7u8yttqvOwSHpmwShf7BbDR8EGvg="


config :: Credentials -> Config
config = Config "https://uat-us-api.experian.com/" "2912775" "0B"


application :: Application
application = Application
  { phone = Valid "8014139377"
  , firstName = "Timothy"
  , middleName = Just "A"
  , lastName = "Applegate"
  , email = "tim@apples.com"
  , ssn = Valid "666204234"
  , dateOfBirth = day "1937-11-01"
  , address = Address
      { street1 = "2209 KINGSTON DR"
      , street2 = Nothing
      , city = "LAWRENCE"
      , state = Valid "KS"
      , postalCode = Valid "660491614"
      }
  }




body :: ByteString
body = [here|
{"payload":{"control":[{"value":"06.00","option":"PIDXML_VERSION"},{"value":"TBD1","option":"SUBSCRIBER_PREAMBLE"},{"value":"TA","option":"SUBSCRIBER_OPERATOR_INITIAL"},{"value":"2913331","option":"SUBSCRIBER_SUB_CODE"},{"value":"timely_demo","option":"PID_USERNAME"},{"value":"MjAxOUFkdmFuY2Ux","option":"PID_PASSWORD"},{"value":"Y","option":"VERBOSE"},{"value":"6","option":"PRODUCT_OPTION"},{"value":"D","option":"DETAIL_REQUEST"},{"value":"123","option":"VENDOR"},{"value":"11","option":"VENDOR_VERSION"},{"value":"","option":"BROKER_NUMBER"},{"value":"","option":"END_USER"},{"value":"","option":"FREEZE_KEY_PIN"}],"application":{"applicants":[{"applicantType":"CO_APPLICANT","contactId":"APPLICANT_CONTACT_ID_1"}],"productDetails":{"productType":"WRITTEN_INSTRUCTIONS"}},"contacts":[{"telephones":[{"id":"Main_Phone_0","number":"+1 6165311574"}],"person":{"personDetails":{"noOfDependents":"","dateOfBirth":"1949-12-09","age":"","gender":"","mothersMaidenName":"","spouseName":"","occupancyStatus":"","yearOfBirth":""},"typeOfPerson":"","names":[{"firstName":"ROGER","id":"","nameSuffix":"","surName":"STANLEY","middleNames":"D"}],"personIdentifier":""},"addresses":[{"postTown":"GRAND RAPIDS","stateProvinceCode":"MI","street2":"","street":"100 50TH ST SW APT 125","postal":"49548","id":"Main_Contact_Address_0","addressType":"CURRENT","poBoxNumber":""}],"emails":[{"email":"John.Smith@Experian.com","id":"MAIN_EMAIL_0","type":""}],"identityDocuments":[{"documentNumber":"666542396","documentType":"SSN","hashedDocumentNumber":""},{"documentNumber":"S550792603937","documentType":"DRIVER_LICENSE","hashedDocumentNumber":""}],"id":"APPLICANT_CONTACT_ID_1"}]},"header":{"txnId":"","messageTime":"2020-07-23T15:45:01Z","tenantId":"W91JGCZD","requestType":"PreciseIdOnly","clientReferenceId":"InitialRequest_0617-04","options":{"modelType":"","useSimulatorMappers":"","skipHMAC":"true","strategyManager":"","workflow":""},"expRequestId":""}}
|]

bodyNewline :: ByteString
bodyNewline = [here|
{
  "payload":{
    "control":[
      {
        "value":"06.00",
        "option":"PIDXML_VERSION"
      },
      {
        "value":"TBD1",
        "option":"SUBSCRIBER_PREAMBLE"
      },
      {
        "value":"TA",
        "option":"SUBSCRIBER_OPERATOR_INITIAL"
      },
      {
        "value":"2913331",
        "option":"SUBSCRIBER_SUB_CODE"
      },
      {
        "value":"timely_demo",
        "option":"PID_USERNAME"
      },
      {
        "value":"MjAxOUFkdmFuY2Ux",
        "option":"PID_PASSWORD"
      },
      {
        "value":"Y",
        "option":"VERBOSE"
      },
      {
        "value":"6",
        "option":"PRODUCT_OPTION"
      },
      {
        "value":"D",
        "option":"DETAIL_REQUEST"
      },
      {
        "value":"123",
        "option":"VENDOR"
      },
      {
        "value":"11",
        "option":"VENDOR_VERSION"
      },
      {
        "value":"",
        "option":"BROKER_NUMBER"
      },
      {
        "value":"",
        "option":"END_USER"
      },
      {
        "value":"",
        "option":"FREEZE_KEY_PIN"
      }
    ],
    "application":{
      "applicants":[
        {
          "applicantType":"CO_APPLICANT",
          "contactId":"APPLICANT_CONTACT_ID_1"
        }
      ],
      "productDetails":{
        "productType":"WRITTEN_INSTRUCTIONS"
      }
    },
    "contacts":[
      {
        "telephones":[
          {
            "id":"Main_Phone_0",
            "number":"+1 6165311574"
          }
        ],
        "person":{
          "personDetails":{
            "noOfDependents":"",
            "dateOfBirth":"1949-12-09",
            "age":"",
            "gender":"",
            "mothersMaidenName":"",
            "spouseName":"",
            "occupancyStatus":"",
            "yearOfBirth":""
          },
          "typeOfPerson":"",
          "names":[
            {
              "firstName":"ROGER",
              "id":"",
              "nameSuffix":"",
              "surName":"STANLEY",
              "middleNames":"D"
            }
          ],
          "personIdentifier":""
        },
        "addresses":[
          {
            "postTown":"GRAND RAPIDS",
            "stateProvinceCode":"MI",
            "street2":"",
            "street":"100 50TH ST SW APT 125",
            "postal":"49548",
            "id":"Main_Contact_Address_0",
            "addressType":"CURRENT",
            "poBoxNumber":""
          }
        ],
        "emails":[
          {
            "email":"John.Smith@Experian.com",
            "id":"MAIN_EMAIL_0",
            "type":""
          }
        ],
        "identityDocuments":[
          {
            "documentNumber":"666542396",
            "documentType":"SSN",
            "hashedDocumentNumber":""
          },
          {
            "documentNumber":"S550792603937",
            "documentType":"DRIVER_LICENSE",
            "hashedDocumentNumber":""
          }
        ],
        "id":"APPLICANT_CONTACT_ID_1"
      }
    ]
  },
  "header":{
    "txnId":"",
    "messageTime":"2020-07-23T15:45:01Z",
    "tenantId":"W91JGCZD",
    "requestType":"PreciseIdOnly",
    "clientReferenceId":"InitialRequest_0617-04",
    "options":{
      "modelType":"",
      "useSimulatorMappers":"",
      "skipHMAC":"true",
      "strategyManager":"",
      "workflow":""
    },
    "expRequestId":""
  }
}
|]
