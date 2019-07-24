{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Test.Underwrite.Experian.Samples where

import Data.ByteString.Lazy (ByteString)
import Data.String.Here

auth :: ByteString
auth = [here|
{
    "issued_at": "1563986736082",
    "expires_in": "1800",
    "token_type": "Bearer",
    "access_token": "eyJraWQiOiJFSmpTMXJQQjdJODBHWjgybmNsSlZPQkF3V3B3ZTVYblNKZUdSZHdpcEY1IiwidHlwIjoiSldUIiwiYWxnIjoiUlMyNTYifQ.eyJzdWIiOiJ0aW1lbHlfYWR2YW5jZV91YXQiLCJFbWFpbCI6InNlYW4uaGVzc0B0aW1lbHlhZHZhbmNlLmNvbSIsIkZpcnN0TmFtZSI6IlNlYW4iLCJpc3MiOiJFWFBFUklBTiIsIkxhc3ROYW1lIjoiSGVzcyIsImV4cCI6MTU2Mzk4ODUzNywiaWF0IjoxNTYzOTg2NzM3LCJqdGkiOiJlMzdkYzNhYi00NTljLTRiNzEtYjcyYS1hNTNjYjBhOWZiZjkifQ.OsAoVUsu4yCwH960XqS4iEk0ffSQL7qaQMNJ6GVfkRIohaA0C8d8dAZMmbRN2sWrzR8U-Ql0A_e8z_diKNLJvXK87QoEs7Dab6yggYzhB_5FbT1rKgnzGuJfQ_KxGP5vazuBxT7UojQy6FN6oqzDXtNjvpAWGk500RTYDUG98ydlS3iMf9NUOZ0w13nab38c9Myrmf4KxJipOspZdnN2XeNcje9HVx8nOTNWG0SAS_cwP5PQN7RdNtc2xQR1wHDU8X4sej50S4-nV75SvTFIZ-loyNM5BFr_tZVe_ivEH8UeMsq7KWW9ONmvLwgdsW55sVhYAGdzvlukSCt3OI8IbA",
    "refresh_token": "7DSsTYer5PeGOUMSbOrg83QoQb4wzobA"
}
|]



crossCore :: ByteString
crossCore = [here|
{
    "responseHeader": {
        "requestType": "PreciseIdOnly",
        "clientReferenceId": "InitialRequest_0617-04",
        "expRequestId": "RB000047270832",
        "messageTime": "2020-07-23T15:45:01Z",
        "overallResponse": {
            "decision": "ACCEPT",
            "decisionText": "Continue",
            "decisionReasons": [
                "PreciseId - Accept"
            ],
            "recommendedNextActions": [],
            "spareObjects": []
        },
        "responseCode": "R0201",
        "responseType": "INFO",
        "responseMessage": "Workflow Complete.",
        "tenantID": "W91JGCZD"
    },
    "clientResponsePayload": {
        "orchestrationDecisions": [
            {
                "sequenceId": "1",
                "decisionSource": "PreciseID",
                "decision": "ACCEPT",
                "decisionReasons": [
                    "PreciseId - Accept"
                ],
                "score": 737,
                "decisionText": "Continue",
                "nextAction": "Continue",
                "appReference": "65384545",
                "decisionTime": "2019-07-24T16:44:52Z"
            }
        ],
        "decisionElements": [
            {
                "serviceName": "PreciseId",
                "applicantId": "APPLICANT_CONTACT_ID_1",
                "decision": "ACC",
                "score": 737,
                "decisionText": "Accept",
                "appReference": "65384545",
                "rules": [
                    {
                        "ruleId": "3401",
                        "ruleName": "glbRule01",
                        "ruleText": "Additional Addresses (0 - 1)"
                    },
                    {
                        "ruleId": "3403",
                        "ruleName": "glbRule02",
                        "ruleText": "Additional Addresses (2 - 3)"
                    }
                ],
                "otherData": {
                    "json": {
                        "fraudSolutions": {
                            "response": {
                                "products": {
                                    "preciseIDServer": {
                                        "sessionID": "A7ORAON32FIM8L3HDKBN0E2K.pidd2v-190724114452530426272",
                                        "header": {
                                            "reportDate": "07242019",
                                            "reportTime": "114452",
                                            "productOption": "06",
                                            "subcode": "2913331",
                                            "referenceNumber": "InitialRequest_0617-04"
                                        },
                                        "messages": {
                                            "message": [
                                                {
                                                    "number": "57",
                                                    "text": "015000    0100",
                                                    "addrMismatch": "N"
                                                }
                                            ],
                                            "consumerStatement": []
                                        },
                                        "summary": {
                                            "transactionID": "65384545",
                                            "initialDecision": "ACC",
                                            "finalDecision": "ACC",
                                            "scores": {
                                                "preciseIDScore": "737",
                                                "preciseIDScorecard": "AC OPEN V2",
                                                "validationScore": "000601",
                                                "validationScorecard": "AC OPEN VAL V2",
                                                "verificationScore": "000770",
                                                "verificationScorecard": "AC OPEN ID THEFT V2",
                                                "complianceDescription": "No Compliance Code",
                                                "reasons": {
                                                    "reason": [
                                                        {
                                                            "value": "Lack of public record information or collection trades indicative that file is susceptible to ID fraud",
                                                            "code": "B110"
                                                        },
                                                        {
                                                            "value": "High credit limits and balances on revolving trades",
                                                            "code": "B105"
                                                        },
                                                        {
                                                            "value": "Average age of revolving trades is too high indicative that file is susceptible of ID fraud",
                                                            "code": "B122"
                                                        },
                                                        {
                                                            "value": "Too many trades with high revolving credit balance to limit ratio",
                                                            "code": "B218"
                                                        },
                                                        {
                                                            "value": "No adverse factor observed",
                                                            "code": "B405"
                                                        }
                                                    ]
                                                },
                                                "fpdscore": "000662",
                                                "fpdscorecard": "AC OPEN FPD V2"
                                            }
                                        },
                                        "preciseMatch": {
                                            "version": "02.00",
                                            "responseStatusCode": {
                                                "value": "Data found for search request",
                                                "code": "00"
                                            },
                                            "preciseMatchTransactionID": "7727e883-e4ca-4b2b-a",
                                            "preciseMatchScore": "749",
                                            "preciseMatchDecision": {
                                                "value": "",
                                                "code": " "
                                            },
                                            "addresses": {
                                                "address": [
                                                    {
                                                        "summary": {
                                                            "verificationResult": {
                                                                "value": "Exact match on first and last name;Exact match on address",
                                                                "code": "A1"
                                                            },
                                                            "type": {
                                                                "value": "Multi-family dwelling",
                                                                "code": "M "
                                                            },
                                                            "unitMismatchResult": {
                                                                "value": "",
                                                                "code": "  "
                                                            },
                                                            "highRiskResult": {
                                                                "value": "No address high risk information found",
                                                                "code": "N "
                                                            },
                                                            "counts": {
                                                                "standardizedAddressReturnCount": 1,
                                                                "residentialAddressMatchCount": 1,
                                                                "residentialAddressReturnCount": 1,
                                                                "highRiskAddressReturnCount": 0,
                                                                "businessAddressMatchCount": 0,
                                                                "businessAddressReturnCount": 0
                                                            }
                                                        },
                                                        "detail": {
                                                            "standardizedAddressRcd": {
                                                                "surname": "STANLEY",
                                                                "firstName": "ROGER",
                                                                "middle": "D",
                                                                "address": "100 50TH ST SW APT 125",
                                                                "city": "GRAND RAPIDS",
                                                                "state": "MI",
                                                                "zipCode": "49548"
                                                            },
                                                            "residentialAddressRcd": [
                                                                {
                                                                    "surname": "STANLEY",
                                                                    "firstName": "ROGER",
                                                                    "middle": "D",
                                                                    "aliasName": [],
                                                                    "address": "100 50TH ST SW APT 125",
                                                                    "city": "GRAND RAPIDS",
                                                                    "state": "MI",
                                                                    "zipCode": "49548",
                                                                    "zipPlus4": "5670",
                                                                    "areaCode": "616",
                                                                    "phone": "5311574",
                                                                    "monthsAtResidence": "0126",
                                                                    "lastUpdatedDate": "19960514"
                                                                }
                                                            ],
                                                            "highRiskAddressRcd": [],
                                                            "highRiskAddressDescription": [
                                                                {
                                                                    "highRiskDescription": "No high risk business at address/phone"
                                                                }
                                                            ],
                                                            "businessAddressRcd": []
                                                        }
                                                    }
                                                ]
                                            },
                                            "phones": {
                                                "phone": [
                                                    {
                                                        "summary": {
                                                            "verificationResult": {
                                                                "value": "No match to Phone",
                                                                "code": "NX"
                                                            },
                                                            "classification": {
                                                                "value": "Phone matches Landline",
                                                                "code": "L"
                                                            },
                                                            "highRiskResult": {
                                                                "value": "No phone high risk information found",
                                                                "code": "N"
                                                            },
                                                            "counts": {
                                                                "residentialPhoneMatchCount": 0,
                                                                "residentialPhoneReturnCount": 0,
                                                                "highRiskPhoneReturnCount": 0,
                                                                "businessPhoneMatchCount": 0,
                                                                "businessPhoneReturnCount": 0
                                                            }
                                                        },
                                                        "detail": {
                                                            "residentialPhoneRcd": [],
                                                            "phoneHighRiskRcd": [],
                                                            "highRiskPhoneDescription": [
                                                                {
                                                                    "highRiskDescription": "No high risk business at address/phone"
                                                                }
                                                            ],
                                                            "businessPhoneRcd": []
                                                        }
                                                    }
                                                ]
                                            },
                                            "consumerID": {
                                                "summary": {
                                                    "verificationResult": {
                                                        "value": "Match to full name and address-Match performed using SSN",
                                                        "code": "YA"
                                                    },
                                                    "deceasedResult": {
                                                        "value": "No",
                                                        "code": "N"
                                                    },
                                                    "formatResult": {
                                                        "value": "Valid",
                                                        "code": "V"
                                                    },
                                                    "issueResult": {
                                                        "value": "SSN issued",
                                                        "code": "I"
                                                    },
                                                    "issueState": "MO",
                                                    "issueStartRange": "1975",
                                                    "issueEndRange": "1977",
                                                    "counts": {
                                                        "consumerIDReturnCount": 1
                                                    }
                                                },
                                                "detail": {
                                                    "consumerIDRcd": [
                                                        {
                                                            "surname": "STANLEY",
                                                            "firstName": "ROGER",
                                                            "middle": "D",
                                                            "aliasName": [],
                                                            "address": "100 50TH ST SW APT 125",
                                                            "city": "GRAND RAPIDS",
                                                            "state": "MI",
                                                            "zipCode": "49548",
                                                            "zipPlus4": "5670",
                                                            "areaCode": "616",
                                                            "phone": "5311574",
                                                            "reportedDate": "19960514",
                                                            "lastUpdatedDate": "20060628"
                                                        }
                                                    ]
                                                }
                                            },
                                            "dateOfBirth": {
                                                "summary": {
                                                    "matchResult": {
                                                        "value": "Day of Birth and Month of Birth exact match, Year of Birth exact match (no plus or minus one year logic accommodation)",
                                                        "code": "9"
                                                    },
                                                    "monthOfBirth": "12",
                                                    "dayOfBirth": "09",
                                                    "yearOfBirth": "1949"
                                                }
                                            },
                                            "driverLicense": {
                                                "summary": {
                                                    "verificationResult": {
                                                        "value": "",
                                                        "code": "YB"
                                                    },
                                                    "formatValidation": {
                                                        "value": "Driver's license not provided or validated",
                                                        "code": " "
                                                    }
                                                }
                                            },
                                            "changeOfAddresses": {
                                                "changeOfAddress": [
                                                    {
                                                        "summary": {
                                                            "verificationResult": {
                                                                "value": "No change of address information found",
                                                                "code": "N "
                                                            },
                                                            "counts": {
                                                                "changeOfAddressReturnCount": 0
                                                            }
                                                        }
                                                    }
                                                ]
                                            },
                                            "ofac": {
                                                "summary": {
                                                    "verificationResult": {
                                                        "value": "No match",
                                                        "code": "1 "
                                                    },
                                                    "counts": {
                                                        "ofacReturnCount": 0
                                                    }
                                                }
                                            },
                                            "previousAddresses": {
                                                "previousAddress": [
                                                    {
                                                        "summary": {
                                                            "counts": {
                                                                "previousAddressReturnCount": 2
                                                            }
                                                        },
                                                        "detail": {
                                                            "previousAddressRcd": [
                                                                {
                                                                    "address": "PO BOX 98D",
                                                                    "city": "ONAWAY",
                                                                    "state": "MI",
                                                                    "zipCode": "49765",
                                                                    "zipPlus4": "0098",
                                                                    "reportedDate": "20060320",
                                                                    "lastUpdatedDate": "20060320"
                                                                },
                                                                {
                                                                    "address": "25140 ROAD N",
                                                                    "city": "CORTEZ",
                                                                    "state": "CO",
                                                                    "zipCode": "81321",
                                                                    "zipPlus4": "8865",
                                                                    "reportedDate": "20040708",
                                                                    "lastUpdatedDate": "20040708"
                                                                }
                                                            ]
                                                        }
                                                    }
                                                ]
                                            },
                                            "ssnfinder": {
                                                "summary": {
                                                    "counts": {
                                                        "ssnfinderReturnCount": 0
                                                    }
                                                }
                                            }
                                        },
                                        "pidxmlversion": "06.00",
                                        "glbDetail": {
                                            "fraudShield": {
                                                "indicator": [
                                                    {
                                                        "value": "N",
                                                        "code": "01"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "02"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "03"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "04"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "05"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "06"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "10"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "11"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "13"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "14"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "15"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "16"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "17"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "18"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "21"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "25"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "26"
                                                    }
                                                ]
                                            },
                                            "glbRules": {
                                                "glbRule": [
                                                    {
                                                        "value": "Additional Addresses (0 - 1)",
                                                        "code": "3401"
                                                    },
                                                    {
                                                        "value": "Additional Addresses (2 - 3)",
                                                        "code": "3403"
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    }
                                                ]
                                            }
                                        },
                                        "fcraDetail": {
                                            "fraudShield": {
                                                "indicator": [
                                                    {
                                                        "value": "N",
                                                        "code": "07"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "08"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "09"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "19"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "20"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "22"
                                                    },
                                                    {
                                                        "value": "N",
                                                        "code": "27"
                                                    }
                                                ]
                                            },
                                            "adverseActions": {
                                                "adverseAction": [
                                                    {
                                                        "value": "",
                                                        "code": " "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": " "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": " "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": " "
                                                    }
                                                ]
                                            },
                                            "fcrarules": {
                                                "fcrarule": [
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    },
                                                    {
                                                        "value": "",
                                                        "code": "    "
                                                    }
                                                ]
                                            }
                                        }
                                    },
                                    "customerManagement": {
                                        "error": {
                                            "reportDate": "07242019",
                                            "reportTime": "114452",
                                            "surname": "STANLEY",
                                            "firstName": "ROG",
                                            "errorCode": "733",
                                            "errorDescription": "Subcode/Product Option configuration does not exist.",
                                            "actionIndicator": {
                                                "value": "Report condition",
                                                "code": "R"
                                            },
                                            "referenceNumber": "InitialRequest_0617-04"
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                "decisions": [
                    {
                        "element": "messageNumber",
                        "value": "57",
                        "reason": "015000    0100"
                    },
                    {
                        "element": "initialDecision",
                        "value": "ACC",
                        "reason": ""
                    },
                    {
                        "element": "finalDecision",
                        "value": "ACC",
                        "reason": ""
                    },
                    {
                        "element": "pidScoreReason1",
                        "value": "B110",
                        "reason": "Lack of public record information or collection trades indicative that file is susceptible to ID fraud"
                    },
                    {
                        "element": "pidScoreReason2",
                        "value": "B105",
                        "reason": "High credit limits and balances on revolving trades"
                    },
                    {
                        "element": "pidScoreReason3",
                        "value": "B122",
                        "reason": "Average age of revolving trades is too high indicative that file is susceptible of ID fraud"
                    },
                    {
                        "element": "pidScoreReason4",
                        "value": "B218",
                        "reason": "Too many trades with high revolving credit balance to limit ratio"
                    },
                    {
                        "element": "pidScoreReason5",
                        "value": "B405",
                        "reason": "No adverse factor observed"
                    }
                ],
                "matches": [
                    {
                        "name": "pmAddressVerificationResult1",
                        "value": "A1"
                    },
                    {
                        "name": "pmPhoneVerificationResult1",
                        "value": "NX"
                    },
                    {
                        "name": "pmConsumerIDVerificationResult",
                        "value": "YA"
                    },
                    {
                        "name": "pmDateOfBirthMatchResult",
                        "value": "9"
                    },
                    {
                        "name": "pmDriverLicenseVerificationResult",
                        "value": "YB"
                    },
                    {
                        "name": "pmChangeOfAddressVerificationResult1",
                        "value": "N"
                    },
                    {
                        "name": "pmOFACVerificationResult",
                        "value": "1"
                    },
                    {
                        "name": "fcraFSIndicator07",
                        "value": "N"
                    },
                    {
                        "name": "fcraFSIndicator08",
                        "value": "N"
                    },
                    {
                        "name": "fcraFSIndicator09",
                        "value": "N"
                    },
                    {
                        "name": "fcraFSIndicator19",
                        "value": "N"
                    },
                    {
                        "name": "fcraFSIndicator20",
                        "value": "N"
                    },
                    {
                        "name": "fcraFSIndicator22",
                        "value": "N"
                    },
                    {
                        "name": "fcraFSIndicator27",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator01",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator02",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator03",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator04",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator05",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator06",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator10",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator11",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator13",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator14",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator15",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator16",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator17",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator18",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator21",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator25",
                        "value": "N"
                    },
                    {
                        "name": "glbFSIndicator26",
                        "value": "N"
                    }
                ],
                "scores": [
                    {
                        "name": "preciseIDScore",
                        "score": 737,
                        "type": "score"
                    },
                    {
                        "name": "validationScore",
                        "score": 601,
                        "type": "score"
                    },
                    {
                        "name": "verificationScore",
                        "score": 770,
                        "type": "score"
                    },
                    {
                        "name": "fpdScore",
                        "score": 662,
                        "type": "score"
                    },
                    {
                        "name": "preciseMatchScore",
                        "score": 749,
                        "type": "score"
                    }
                ]
            }
        ]
    },
    "originalRequestData": {
        "application": {
            "applicants": [
                {
                    "applicantType": "CO_APPLICANT",
                    "contactId": "APPLICANT_CONTACT_ID_1"
                }
            ],
            "productDetails": {
                "productType": "WRITTEN_INSTRUCTIONS"
            }
        },
        "contacts": [
            {
                "addresses": [
                    {
                        "addressType": "CURRENT",
                        "id": "Main_Contact_Address_0",
                        "poBoxNumber": "",
                        "postTown": "GRAND RAPIDS",
                        "postal": "49548",
                        "stateProvinceCode": "MI",
                        "street": "100 50TH ST SW APT 125",
                        "street2": ""
                    }
                ],
                "emails": [
                    {
                        "email": "John.Smith@Experian.com",
                        "id": "MAIN_EMAIL_0",
                        "type": ""
                    }
                ],
                "id": "APPLICANT_CONTACT_ID_1",
                "identityDocuments": [
                    {
                        "documentNumber": "666542396",
                        "documentType": "SSN",
                        "hashedDocumentNumber": ""
                    },
                    {
                        "documentNumber": "S550792603937",
                        "documentType": "DRIVER_LICENSE",
                        "hashedDocumentNumber": ""
                    }
                ],
                "person": {
                    "names": [
                        {
                            "firstName": "ROGER",
                            "id": "",
                            "middleNames": "D",
                            "nameSuffix": "",
                            "surName": "STANLEY"
                        }
                    ],
                    "personDetails": {
                        "age": "",
                        "dateOfBirth": "1949-12-09",
                        "gender": "",
                        "mothersMaidenName": "",
                        "noOfDependents": "",
                        "occupancyStatus": "",
                        "spouseName": "",
                        "yearOfBirth": ""
                    },
                    "personIdentifier": "",
                    "typeOfPerson": ""
                },
                "telephones": [
                    {
                        "id": "Main_Phone_0",
                        "number": "+1 6165311574"
                    }
                ]
            }
        ],
        "control": [
            {
                "option": "PIDXML_VERSION",
                "value": "06.00"
            },
            {
                "option": "SUBSCRIBER_PREAMBLE",
                "value": "TBD1"
            },
            {
                "option": "SUBSCRIBER_OPERATOR_INITIAL",
                "value": "TA"
            },
            {
                "option": "SUBSCRIBER_SUB_CODE",
                "value": "2913331"
            },
            {
                "option": "PID_USERNAME",
                "value": "timely_demo"
            },
            {
                "option": "PID_PASSWORD",
                "value": "MjAxOUFkdmFuY2Ux"
            },
            {
                "option": "VERBOSE",
                "value": "Y"
            },
            {
                "option": "PRODUCT_OPTION",
                "value": "6"
            },
            {
                "option": "DETAIL_REQUEST",
                "value": "D"
            },
            {
                "option": "VENDOR",
                "value": "123"
            },
            {
                "option": "VENDOR_VERSION",
                "value": "11"
            },
            {
                "option": "BROKER_NUMBER",
                "value": ""
            },
            {
                "option": "END_USER",
                "value": ""
            },
            {
                "option": "FREEZE_KEY_PIN",
                "value": ""
            }
        ]
    }
}
|]




daas :: ByteString
daas = [here|
{
    "creditProfile": [
        {
            "headerRecord": [
                {
                    "reportDate": "072419",
                    "reportTime": "114638",
                    "preamble": "TBD1",
                    "versionNo": "07",
                    "mKeywordLength": "00"
                }
            ],
            "addressInformation": [
                {
                    "city": "TUSTIN",
                    "dwellingType": "S",
                    "lastReportingSubscriberCode": "1230840",
                    "source": "2",
                    "state": "MI",
                    "streetName": "100TH",
                    "streetPrefix": "21598",
                    "streetSuffix": "AVE",
                    "timesReported": "10",
                    "zipCode": "496888651"
                },
                {
                    "city": "TUSTIN",
                    "dwellingType": "S",
                    "source": "1",
                    "state": "MI",
                    "streetName": "100TH",
                    "streetPrefix": "10620 N",
                    "streetSuffix": "AVE",
                    "timesReported": "00",
                    "zipCode": "49688"
                }
            ],
            "consumerIdentity": {
                "name": [
                    {
                        "firstName": "NELSON",
                        "surname": "LINS",
                        "type": "N"
                    }
                ]
            },
            "riskModel": [
                {
                    "evaluation": "P",
                    "modelIndicator": "V4",
                    "score": "0808",
                    "scoreFactors": [
                        {
                            "importance": "1",
                            "code": "68"
                        },
                        {
                            "importance": "2",
                            "code": "25"
                        },
                        {
                            "importance": "3",
                            "code": "49"
                        },
                        {
                            "importance": "4",
                            "code": "62"
                        }
                    ]
                },
                {
                    "evaluation": "P",
                    "modelIndicator": "F9",
                    "score": "0831",
                    "scoreFactors": [
                        {
                            "importance": "1",
                            "code": "32"
                        },
                        {
                            "importance": "2",
                            "code": "05"
                        },
                        {
                            "importance": "3",
                            "code": "14"
                        }
                    ]
                }
            ],
            "ssn": [
                {
                    "number": "666596325"
                }
            ],
            "tradeline": [
                {
                    "accountNumber": "6203026135655",
                    "accountType": "02",
                    "amount1": "00020000",
                    "amount1Qualifier": "O",
                    "delinquencies30Days": "00",
                    "delinquencies60Days": "00",
                    "delinquencies90to180Days": "00",
                    "derogCounter": "00",
                    "ecoa": "1",
                    "enhancedPaymentData": {
                        "enhancedAccountCondition": "05",
                        "enhancedAccountType": "02",
                        "enhancedPaymentStatus": "11"
                    },
                    "evaluation": "P",
                    "kob": "BB",
                    "monthsHistory": "02",
                    "openOrClosed": "C",
                    "paymentHistory": "BC",
                    "revolvingOrInstallment": "I",
                    "status": "05",
                    "subscriberCode": "2170303",
                    "subscriberName": "OWENSBORO NATIONAL BAN",
                    "terms": "011"
                },
                {
                    "accountNumber": "6203026135656",
                    "accountType": "02",
                    "amount1": "00017000",
                    "amount1Qualifier": "O",
                    "delinquencies30Days": "00",
                    "delinquencies60Days": "00",
                    "delinquencies90to180Days": "00",
                    "derogCounter": "00",
                    "ecoa": "1",
                    "enhancedPaymentData": {
                        "enhancedAccountCondition": "05",
                        "enhancedAccountType": "02",
                        "enhancedPaymentStatus": "11"
                    },
                    "evaluation": "P",
                    "kob": "BB",
                    "monthsHistory": "01",
                    "openOrClosed": "C",
                    "paymentHistory": "B",
                    "revolvingOrInstallment": "I",
                    "status": "05",
                    "subscriberCode": "2170303",
                    "subscriberName": "OWENSBORO NATIONAL BAN",
                    "terms": "003"
                },
                {
                    "accountNumber": "542418010625",
                    "accountType": "18",
                    "amount1": "00005100",
                    "amount1Qualifier": "L",
                    "balanceAmount": "00000000",
                    "delinquencies30Days": "00",
                    "delinquencies60Days": "00",
                    "delinquencies90to180Days": "00",
                    "derogCounter": "00",
                    "ecoa": "1",
                    "enhancedPaymentData": {
                        "enhancedAccountCondition": "A3",
                        "enhancedAccountType": "18",
                        "enhancedPaymentStatus": "11",
                        "enhancedSpecialComment": "19"
                    },
                    "evaluation": "N",
                    "kob": "BC",
                    "monthsHistory": "16",
                    "openOrClosed": "C",
                    "paymentHistory": "B0000--------000",
                    "revolvingOrInstallment": "R",
                    "specialComment": "19",
                    "status": "11",
                    "subscriberCode": "1240000",
                    "subscriberName": "CITICARDS CBNA",
                    "terms": "REV"
                },
                {
                    "accountNumber": "890810651094557",
                    "accountType": "02",
                    "amount1": "00017075",
                    "amount1Qualifier": "O",
                    "delinquencies30Days": "00",
                    "delinquencies60Days": "00",
                    "delinquencies90to180Days": "00",
                    "derogCounter": "00",
                    "ecoa": "1",
                    "enhancedPaymentData": {
                        "enhancedAccountCondition": "A2",
                        "enhancedAccountType": "02",
                        "enhancedPaymentStatus": "11"
                    },
                    "evaluation": "P",
                    "kob": "BB",
                    "monthsHistory": "12",
                    "openOrClosed": "C",
                    "paymentHistory": "BCCCCCCCCCCC",
                    "revolvingOrInstallment": "I",
                    "status": "12",
                    "subscriberCode": "1199117",
                    "subscriberName": "BB&T",
                    "terms": "012"
                },
                {
                    "accountNumber": "890810651094559",
                    "accountType": "02",
                    "amount1": "00006075",
                    "amount1Qualifier": "O",
                    "delinquencies30Days": "00",
                    "delinquencies60Days": "00",
                    "delinquencies90to180Days": "00",
                    "derogCounter": "00",
                    "ecoa": "1",
                    "enhancedPaymentData": {
                        "enhancedAccountCondition": "A2",
                        "enhancedAccountType": "02",
                        "enhancedPaymentStatus": "11"
                    },
                    "evaluation": "P",
                    "kob": "BB",
                    "monthsHistory": "07",
                    "openOrClosed": "C",
                    "paymentHistory": "BCCCCCC",
                    "revolvingOrInstallment": "I",
                    "status": "12",
                    "subscriberCode": "1199117",
                    "subscriberName": "BB&T",
                    "terms": "012"
                },
                {
                    "accountNumber": "890810651094556",
                    "accountType": "78",
                    "amount1": "00016143",
                    "amount1Qualifier": "O",
                    "delinquencies30Days": "00",
                    "delinquencies60Days": "00",
                    "delinquencies90to180Days": "00",
                    "derogCounter": "00",
                    "ecoa": "1",
                    "enhancedPaymentData": {
                        "enhancedAccountCondition": "A2",
                        "enhancedAccountType": "78",
                        "enhancedPaymentStatus": "11"
                    },
                    "evaluation": "P",
                    "kob": "BB",
                    "monthsHistory": "11",
                    "openOrClosed": "C",
                    "paymentHistory": "BCCCCCCCCCC",
                    "revolvingOrInstallment": "I",
                    "status": "12",
                    "subscriberCode": "1199117",
                    "subscriberName": "BB&T",
                    "terms": "011"
                },
                {
                    "accountNumber": "890810651093556",
                    "accountType": "78",
                    "amount1": "00020000",
                    "amount1Qualifier": "O",
                    "delinquencies30Days": "00",
                    "delinquencies60Days": "00",
                    "delinquencies90to180Days": "00",
                    "derogCounter": "00",
                    "ecoa": "1",
                    "enhancedPaymentData": {
                        "enhancedAccountCondition": "A2",
                        "enhancedAccountType": "78",
                        "enhancedPaymentStatus": "11"
                    },
                    "evaluation": "P",
                    "kob": "BB",
                    "monthsHistory": "10",
                    "openOrClosed": "C",
                    "paymentHistory": "BCCCCCCCCC",
                    "revolvingOrInstallment": "I",
                    "status": "12",
                    "subscriberCode": "1199117",
                    "subscriberName": "BB&T",
                    "terms": "011"
                },
                {
                    "accountNumber": "890810651093557",
                    "accountType": "78",
                    "amount1": "00017000",
                    "amount1Qualifier": "O",
                    "delinquencies30Days": "00",
                    "delinquencies60Days": "00",
                    "delinquencies90to180Days": "00",
                    "derogCounter": "00",
                    "ecoa": "1",
                    "enhancedPaymentData": {
                        "enhancedAccountCondition": "A2",
                        "enhancedAccountType": "78",
                        "enhancedPaymentStatus": "11"
                    },
                    "evaluation": "P",
                    "kob": "BB",
                    "monthsHistory": "03",
                    "openOrClosed": "C",
                    "paymentHistory": "BCC",
                    "revolvingOrInstallment": "I",
                    "status": "12",
                    "subscriberCode": "1199117",
                    "subscriberName": "BB&T",
                    "terms": "003"
                },
                {
                    "accountNumber": "13167205657",
                    "accountType": "02",
                    "amount1": "00020000",
                    "amount1Qualifier": "O",
                    "delinquencies30Days": "00",
                    "delinquencies60Days": "00",
                    "delinquencies90to180Days": "00",
                    "derogCounter": "00",
                    "ecoa": "1",
                    "enhancedPaymentData": {
                        "enhancedAccountType": "02",
                        "enhancedPaymentStatus": "A2"
                    },
                    "evaluation": "P",
                    "kob": "BB",
                    "monthsHistory": "01",
                    "openOrClosed": "C",
                    "paymentHistory": "B",
                    "revolvingOrInstallment": "I",
                    "status": "13",
                    "subscriberCode": "2170303",
                    "subscriberName": "OWENSBORO NATIONAL BAN",
                    "terms": "UNK"
                },
                {
                    "accountNumber": "402411800737",
                    "accountType": "18",
                    "amount1": "00013500",
                    "amount1Qualifier": "L",
                    "amount2": "00003061",
                    "amount2Qualifier": "H",
                    "balanceAmount": "00000248",
                    "delinquencies30Days": "00",
                    "delinquencies60Days": "00",
                    "delinquencies90to180Days": "00",
                    "derogCounter": "00",
                    "ecoa": "1",
                    "enhancedPaymentData": {
                        "enhancedAccountCondition": "A1",
                        "enhancedAccountType": "18",
                        "enhancedPaymentStatus": "11"
                    },
                    "evaluation": "P",
                    "kob": "BC",
                    "monthlyPaymentAmount": "00000010",
                    "monthlyPaymentType": "S",
                    "monthsHistory": "48",
                    "openOrClosed": "O",
                    "paymentHistory": "CCCCCCCCCCCCCCCCCCCCCCCCC",
                    "revolvingOrInstallment": "R",
                    "status": "11",
                    "subscriberCode": "3202754",
                    "subscriberName": "BANK OF AMERICA",
                    "terms": "REV"
                },
                {
                    "accountNumber": "7982220180170348",
                    "accountType": "07",
                    "amount1": "00007500",
                    "amount1Qualifier": "L",
                    "amount2": "00002259",
                    "amount2Qualifier": "H",
                    "balanceAmount": "00000000",
                    "delinquencies30Days": "00",
                    "delinquencies60Days": "00",
                    "delinquencies90to180Days": "00",
                    "derogCounter": "00",
                    "ecoa": "1",
                    "enhancedPaymentData": {
                        "enhancedAccountCondition": "A1",
                        "enhancedAccountType": "07",
                        "enhancedPaymentStatus": "11"
                    },
                    "evaluation": "P",
                    "kob": "LZ",
                    "monthsHistory": "65",
                    "openOrClosed": "O",
                    "paymentHistory": "000CCCCCCCCC00000000000--",
                    "revolvingOrInstallment": "R",
                    "status": "11",
                    "subscriberCode": "1607340",
                    "subscriberName": "SYNCB/LOWES",
                    "terms": "REV"
                }
            ],
            "premierAttributes": [
                {
                    "id": "ALJ0300",
                    "value": "000000000"
                },
                {
                    "id": "ALJ0316",
                    "value": "000000098"
                },
                {
                    "id": "ALJ0416",
                    "value": "000000098"
                },
                {
                    "id": "ALJ5030",
                    "value": "999999998"
                },
                {
                    "id": "ALJ5320",
                    "value": "999999998"
                },
                {
                    "id": "ALJ5730",
                    "value": "999999998"
                },
                {
                    "id": "ALJ5820",
                    "value": "999999998"
                },
                {
                    "id": "ALJ5830",
                    "value": "999999998"
                },
                {
                    "id": "ALJ8120",
                    "value": "000009998"
                },
                {
                    "id": "ALJ8220",
                    "value": "000009998"
                },
                {
                    "id": "ALL0000",
                    "value": "000000011"
                },
                {
                    "id": "ALL0060",
                    "value": "000000096"
                },
                {
                    "id": "ALL0061",
                    "value": "000000096"
                },
                {
                    "id": "ALL0100",
                    "value": "000000011"
                },
                {
                    "id": "ALL0101",
                    "value": "000000000"
                },
                {
                    "id": "ALL0102",
                    "value": "000000000"
                },
                {
                    "id": "ALL0133",
                    "value": "000000000"
                },
                {
                    "id": "ALL0135",
                    "value": "000000000"
                },
                {
                    "id": "ALL0136",
                    "value": "000000000"
                },
                {
                    "id": "ALL0200",
                    "value": "000000009"
                },
                {
                    "id": "ALL0201",
                    "value": "000000000"
                },
                {
                    "id": "ALL0206",
                    "value": "000000000"
                },
                {
                    "id": "ALL0216",
                    "value": "000000002"
                },
                {
                    "id": "ALL0217",
                    "value": "000000004"
                },
                {
                    "id": "ALL0218",
                    "value": "000000004"
                },
                {
                    "id": "ALL0300",
                    "value": "000000009"
                },
                {
                    "id": "ALL0302",
                    "value": "000000000"
                },
                {
                    "id": "ALL0303",
                    "value": "000000000"
                },
                {
                    "id": "ALL0305",
                    "value": "000000000"
                },
                {
                    "id": "ALL0306",
                    "value": "000000000"
                },
                {
                    "id": "ALL0316",
                    "value": "000000002"
                },
                {
                    "id": "ALL0317",
                    "value": "000000004"
                },
                {
                    "id": "ALL0318",
                    "value": "000000004"
                },
                {
                    "id": "ALL0336",
                    "value": "000000000"
                },
                {
                    "id": "ALL0337",
                    "value": "000000000"
                },
                {
                    "id": "ALL0400",
                    "value": "000000002"
                },
                {
                    "id": "ALL0416",
                    "value": "000000002"
                },
                {
                    "id": "ALL0436",
                    "value": "000000000"
                },
                {
                    "id": "ALL0437",
                    "value": "000000000"
                },
                {
                    "id": "ALL0438",
                    "value": "000000000"
                },
                {
                    "id": "ALL0439",
                    "value": "000000000"
                },
                {
                    "id": "ALL0446",
                    "value": "000000002"
                },
                {
                    "id": "ALL0448",
                    "value": "000000002"
                },
                {
                    "id": "ALL0700",
                    "value": "000000007"
                },
                {
                    "id": "ALL0716",
                    "value": "000000000"
                },
                {
                    "id": "ALL0726",
                    "value": "000000000"
                },
                {
                    "id": "ALL0900",
                    "value": "000000000"
                },
                {
                    "id": "ALL1300",
                    "value": "000000009"
                },
                {
                    "id": "ALL1302",
                    "value": "000000000"
                },
                {
                    "id": "ALL1303",
                    "value": "000000000"
                },
                {
                    "id": "ALL1306",
                    "value": "000000000"
                },
                {
                    "id": "ALL1360",
                    "value": "000000002"
                },
                {
                    "id": "ALL1361",
                    "value": "000000000"
                },
                {
                    "id": "ALL1370",
                    "value": "000000004"
                },
                {
                    "id": "ALL1371",
                    "value": "000000000"
                },
                {
                    "id": "ALL1380",
                    "value": "000000004"
                },
                {
                    "id": "ALL1401",
                    "value": "000000001"
                },
                {
                    "id": "ALL1760",
                    "value": "000000000"
                },
                {
                    "id": "ALL2000",
                    "value": "000000002"
                },
                {
                    "id": "ALL2001",
                    "value": "000000096"
                },
                {
                    "id": "ALL2002",
                    "value": "000000096"
                },
                {
                    "id": "ALL2003",
                    "value": "000000096"
                },
                {
                    "id": "ALL2004",
                    "value": "000000096"
                },
                {
                    "id": "ALL2005",
                    "value": "000000002"
                },
                {
                    "id": "ALL2006",
                    "value": "000000096"
                },
                {
                    "id": "ALL2008",
                    "value": "000000096"
                },
                {
                    "id": "ALL2009",
                    "value": "000000096"
                },
                {
                    "id": "ALL2011",
                    "value": "000000096"
                },
                {
                    "id": "ALL2012",
                    "value": "000000096"
                },
                {
                    "id": "ALL2106",
                    "value": "000000000"
                },
                {
                    "id": "ALL2116",
                    "value": "000000000"
                },
                {
                    "id": "ALL2120",
                    "value": "000000000"
                },
                {
                    "id": "ALL2126",
                    "value": "000000000"
                },
                {
                    "id": "ALL2136",
                    "value": "000000000"
                },
                {
                    "id": "ALL2146",
                    "value": "000000000"
                },
                {
                    "id": "ALL2166",
                    "value": "000000000"
                },
                {
                    "id": "ALL2176",
                    "value": "000000000"
                },
                {
                    "id": "ALL2196",
                    "value": "000000000"
                },
                {
                    "id": "ALL2201",
                    "value": "000000000"
                },
                {
                    "id": "ALL2202",
                    "value": "000000000"
                },
                {
                    "id": "ALL2206",
                    "value": "000000000"
                },
                {
                    "id": "ALL2216",
                    "value": "000000000"
                },
                {
                    "id": "ALL2220",
                    "value": "000000000"
                },
                {
                    "id": "ALL2226",
                    "value": "000000000"
                },
                {
                    "id": "ALL2300",
                    "value": "000000000"
                },
                {
                    "id": "ALL2306",
                    "value": "000000000"
                },
                {
                    "id": "ALL2307",
                    "value": "000000000"
                },
                {
                    "id": "ALL2308",
                    "value": "000000000"
                },
                {
                    "id": "ALL2309",
                    "value": "000000000"
                },
                {
                    "id": "ALL2320",
                    "value": "000000000"
                },
                {
                    "id": "ALL2321",
                    "value": "000000000"
                },
                {
                    "id": "ALL2322",
                    "value": "000000000"
                },
                {
                    "id": "ALL2326",
                    "value": "000000000"
                },
                {
                    "id": "ALL2327",
                    "value": "000000000"
                },
                {
                    "id": "ALL2328",
                    "value": "000000000"
                },
                {
                    "id": "ALL2330",
                    "value": "000000000"
                },
                {
                    "id": "ALL2336",
                    "value": "000000000"
                },
                {
                    "id": "ALL2337",
                    "value": "000000000"
                },
                {
                    "id": "ALL2338",
                    "value": "000000000"
                },
                {
                    "id": "ALL2339",
                    "value": "000000000"
                },
                {
                    "id": "ALL2350",
                    "value": "000000000"
                },
                {
                    "id": "ALL2351",
                    "value": "000000000"
                },
                {
                    "id": "ALL2352",
                    "value": "000000000"
                },
                {
                    "id": "ALL2356",
                    "value": "000000000"
                },
                {
                    "id": "ALL2357",
                    "value": "000000000"
                },
                {
                    "id": "ALL2358",
                    "value": "000000000"
                },
                {
                    "id": "ALL2360",
                    "value": "000000000"
                },
                {
                    "id": "ALL2366",
                    "value": "000000000"
                },
                {
                    "id": "ALL2367",
                    "value": "000000000"
                },
                {
                    "id": "ALL2368",
                    "value": "000000000"
                },
                {
                    "id": "ALL2369",
                    "value": "000000000"
                },
                {
                    "id": "ALL2380",
                    "value": "000000000"
                },
                {
                    "id": "ALL2386",
                    "value": "000000000"
                },
                {
                    "id": "ALL2387",
                    "value": "000000000"
                },
                {
                    "id": "ALL2388",
                    "value": "000000000"
                },
                {
                    "id": "ALL2390",
                    "value": "000000000"
                },
                {
                    "id": "ALL2420",
                    "value": "000000000"
                },
                {
                    "id": "ALL2421",
                    "value": "000000000"
                },
                {
                    "id": "ALL2422",
                    "value": "000000000"
                },
                {
                    "id": "ALL2423",
                    "value": "000000000"
                },
                {
                    "id": "ALL2427",
                    "value": "000000000"
                },
                {
                    "id": "ALL2428",
                    "value": "000000000"
                },
                {
                    "id": "ALL2450",
                    "value": "000000000"
                },
                {
                    "id": "ALL2456",
                    "value": "000000000"
                },
                {
                    "id": "ALL2457",
                    "value": "000000000"
                },
                {
                    "id": "ALL2458",
                    "value": "000000000"
                },
                {
                    "id": "ALL2480",
                    "value": "000000000"
                },
                {
                    "id": "ALL2486",
                    "value": "000000000"
                },
                {
                    "id": "ALL2487",
                    "value": "000000000"
                },
                {
                    "id": "ALL2488",
                    "value": "000000000"
                },
                {
                    "id": "ALL2490",
                    "value": "000000000"
                },
                {
                    "id": "ALL2700",
                    "value": "000000000"
                },
                {
                    "id": "ALL2702",
                    "value": "000000000"
                },
                {
                    "id": "ALL2703",
                    "value": "000000000"
                },
                {
                    "id": "ALL2704",
                    "value": "000000000"
                },
                {
                    "id": "ALL2706",
                    "value": "000000000"
                },
                {
                    "id": "ALL2707",
                    "value": "000000000"
                },
                {
                    "id": "ALL2708",
                    "value": "000000000"
                },
                {
                    "id": "ALL2720",
                    "value": "000000000"
                },
                {
                    "id": "ALL2721",
                    "value": "000000000"
                },
                {
                    "id": "ALL2800",
                    "value": "000000000"
                },
                {
                    "id": "ALL2830",
                    "value": "000000000"
                },
                {
                    "id": "ALL2831",
                    "value": "000000000"
                },
                {
                    "id": "ALL2840",
                    "value": "000000000"
                },
                {
                    "id": "ALL2841",
                    "value": "000000000"
                },
                {
                    "id": "ALL2870",
                    "value": "000000000"
                },
                {
                    "id": "ALL2874",
                    "value": "000000000"
                },
                {
                    "id": "ALL2875",
                    "value": "000000000"
                },
                {
                    "id": "ALL2880",
                    "value": "000000000"
                },
                {
                    "id": "ALL2900",
                    "value": "000000000"
                },
                {
                    "id": "ALL2906",
                    "value": "000000000"
                },
                {
                    "id": "ALL2907",
                    "value": "000000000"
                },
                {
                    "id": "ALL2908",
                    "value": "000000000"
                },
                {
                    "id": "ALL2910",
                    "value": "000000000"
                },
                {
                    "id": "ALL2916",
                    "value": "000000000"
                },
                {
                    "id": "ALL2917",
                    "value": "000000000"
                },
                {
                    "id": "ALL2918",
                    "value": "000000000"
                },
                {
                    "id": "ALL2919",
                    "value": "000000000"
                },
                {
                    "id": "ALL2930",
                    "value": "000000000"
                },
                {
                    "id": "ALL2936",
                    "value": "000000000"
                },
                {
                    "id": "ALL2937",
                    "value": "000000000"
                },
                {
                    "id": "ALL2938",
                    "value": "000000000"
                },
                {
                    "id": "ALL2940",
                    "value": "000000000"
                },
                {
                    "id": "ALL2946",
                    "value": "000000000"
                },
                {
                    "id": "ALL2947",
                    "value": "000000000"
                },
                {
                    "id": "ALL2948",
                    "value": "000000000"
                },
                {
                    "id": "ALL2949",
                    "value": "000000000"
                },
                {
                    "id": "ALL2960",
                    "value": "000000000"
                },
                {
                    "id": "ALL2966",
                    "value": "000000000"
                },
                {
                    "id": "ALL2967",
                    "value": "000000000"
                },
                {
                    "id": "ALL2968",
                    "value": "000000000"
                },
                {
                    "id": "ALL2970",
                    "value": "000000000"
                },
                {
                    "id": "ALL2976",
                    "value": "000000000"
                },
                {
                    "id": "ALL2977",
                    "value": "000000000"
                },
                {
                    "id": "ALL2978",
                    "value": "000000000"
                },
                {
                    "id": "ALL2979",
                    "value": "000000000"
                },
                {
                    "id": "ALL2990",
                    "value": "000000000"
                },
                {
                    "id": "ALL2996",
                    "value": "000000000"
                },
                {
                    "id": "ALL2997",
                    "value": "000000000"
                },
                {
                    "id": "ALL2998",
                    "value": "000000000"
                },
                {
                    "id": "ALL2999",
                    "value": "000000000"
                },
                {
                    "id": "ALL3110",
                    "value": "000000001"
                },
                {
                    "id": "ALL3202",
                    "value": "000000000"
                },
                {
                    "id": "ALL3311",
                    "value": "000000000"
                },
                {
                    "id": "ALL3410",
                    "value": "000000001"
                },
                {
                    "id": "ALL3412",
                    "value": "000000000"
                },
                {
                    "id": "ALL3446",
                    "value": "000000002"
                },
                {
                    "id": "ALL3510",
                    "value": "000000001"
                },
                {
                    "id": "ALL3517",
                    "value": "000000000"
                },
                {
                    "id": "ALL4000",
                    "value": "000000000"
                },
                {
                    "id": "ALL4001",
                    "value": "000000000"
                },
                {
                    "id": "ALL4018",
                    "value": "000000032"
                },
                {
                    "id": "ALL4028",
                    "value": "000000032"
                },
                {
                    "id": "ALL4060",
                    "value": "000000000"
                },
                {
                    "id": "ALL4070",
                    "value": "000000000"
                },
                {
                    "id": "ALL4080",
                    "value": "000000000"
                },
                {
                    "id": "ALL4090",
                    "value": "000000000"
                },
                {
                    "id": "ALL4100",
                    "value": "000000000"
                },
                {
                    "id": "ALL4160",
                    "value": "000000000"
                },
                {
                    "id": "ALL4170",
                    "value": "000000000"
                },
                {
                    "id": "ALL4180",
                    "value": "000000000"
                },
                {
                    "id": "ALL4190",
                    "value": "000000000"
                },
                {
                    "id": "ALL4300",
                    "value": "000000000"
                },
                {
                    "id": "ALL4360",
                    "value": "000000000"
                },
                {
                    "id": "ALL4370",
                    "value": "000000000"
                },
                {
                    "id": "ALL4380",
                    "value": "000000000"
                },
                {
                    "id": "ALL4390",
                    "value": "000000000"
                },
                {
                    "id": "ALL4400",
                    "value": "000000000"
                },
                {
                    "id": "ALL4460",
                    "value": "000000000"
                },
                {
                    "id": "ALL4470",
                    "value": "000000000"
                },
                {
                    "id": "ALL4480",
                    "value": "000000000"
                },
                {
                    "id": "ALL4490",
                    "value": "000000000"
                },
                {
                    "id": "ALL4520",
                    "value": "000000047"
                },
                {
                    "id": "ALL4600",
                    "value": "000000000"
                },
                {
                    "id": "ALL4660",
                    "value": "000000000"
                },
                {
                    "id": "ALL4670",
                    "value": "000000000"
                },
                {
                    "id": "ALL4680",
                    "value": "000000000"
                },
                {
                    "id": "ALL4690",
                    "value": "000000000"
                },
                {
                    "id": "ALL4700",
                    "value": "000000000"
                },
                {
                    "id": "ALL4760",
                    "value": "000000000"
                },
                {
                    "id": "ALL4770",
                    "value": "000000000"
                },
                {
                    "id": "ALL4780",
                    "value": "000000000"
                },
                {
                    "id": "ALL4790",
                    "value": "000000000"
                },
                {
                    "id": "ALL4980",
                    "value": "000000096"
                },
                {
                    "id": "ALL5012",
                    "value": "000000000"
                },
                {
                    "id": "ALL5015",
                    "value": "000000000"
                },
                {
                    "id": "ALL5018",
                    "value": "000000000"
                },
                {
                    "id": "ALL5020",
                    "value": "000000248"
                },
                {
                    "id": "ALL5030",
                    "value": "000000248"
                },
                {
                    "id": "ALL5040",
                    "value": "000000000"
                },
                {
                    "id": "ALL5042",
                    "value": "000000000"
                },
                {
                    "id": "ALL5043",
                    "value": "000000000"
                },
                {
                    "id": "ALL5045",
                    "value": "000000000"
                },
                {
                    "id": "ALL5047",
                    "value": "000000000"
                },
                {
                    "id": "ALL5048",
                    "value": "000000000"
                },
                {
                    "id": "ALL5070",
                    "value": "000000000"
                },
                {
                    "id": "ALL5071",
                    "value": "000000000"
                },
                {
                    "id": "ALL5072",
                    "value": "000000000"
                },
                {
                    "id": "ALL5073",
                    "value": "000000000"
                },
                {
                    "id": "ALL5074",
                    "value": "000000000"
                },
                {
                    "id": "ALL5075",
                    "value": "000000000"
                },
                {
                    "id": "ALL5120",
                    "value": "000000248"
                },
                {
                    "id": "ALL5301",
                    "value": "000000000"
                },
                {
                    "id": "ALL5320",
                    "value": "000021000"
                },
                {
                    "id": "ALL5321",
                    "value": "000021000"
                },
                {
                    "id": "ALL5360",
                    "value": "000000000"
                },
                {
                    "id": "ALL5361",
                    "value": "000000000"
                },
                {
                    "id": "ALL5420",
                    "value": "000013500"
                },
                {
                    "id": "ALL5460",
                    "value": "000000000"
                },
                {
                    "id": "ALL5461",
                    "value": "000000000"
                },
                {
                    "id": "ALL5740",
                    "value": "000000000"
                },
                {
                    "id": "ALL5742",
                    "value": "000000000"
                },
                {
                    "id": "ALL5743",
                    "value": "000000000"
                },
                {
                    "id": "ALL5747",
                    "value": "000000000"
                },
                {
                    "id": "ALL5770",
                    "value": "000000000"
                },
                {
                    "id": "ALL5820",
                    "value": "000000010"
                },
                {
                    "id": "ALL5825",
                    "value": "000000010"
                },
                {
                    "id": "ALL5830",
                    "value": "000000010"
                },
                {
                    "id": "ALL5835",
                    "value": "000000010"
                },
                {
                    "id": "ALL5935",
                    "value": "000000155"
                },
                {
                    "id": "ALL6100",
                    "value": "000000001"
                },
                {
                    "id": "ALL6120",
                    "value": "000000000"
                },
                {
                    "id": "ALL6160",
                    "value": "000000001"
                },
                {
                    "id": "ALL6190",
                    "value": "000000001"
                },
                {
                    "id": "ALL6200",
                    "value": "000000001"
                },
                {
                    "id": "ALL6203",
                    "value": "000000001"
                },
                {
                    "id": "ALL6210",
                    "value": "000000994"
                },
                {
                    "id": "ALL6220",
                    "value": "000000001"
                },
                {
                    "id": "ALL6230",
                    "value": "000000001"
                },
                {
                    "id": "ALL6250",
                    "value": "000000001"
                },
                {
                    "id": "ALL6270",
                    "value": "000000001"
                },
                {
                    "id": "ALL6280",
                    "value": "000000001"
                },
                {
                    "id": "ALL6310",
                    "value": "000000002"
                },
                {
                    "id": "ALL6320",
                    "value": "000000006"
                },
                {
                    "id": "ALL6400",
                    "value": "000000001"
                },
                {
                    "id": "ALL6460",
                    "value": "000000001"
                },
                {
                    "id": "ALL6500",
                    "value": "000000998"
                },
                {
                    "id": "ALL6600",
                    "value": "000000998"
                },
                {
                    "id": "ALL6700",
                    "value": "000000998"
                },
                {
                    "id": "ALL6800",
                    "value": "000000998"
                },
                {
                    "id": "ALL6900",
                    "value": "000000001"
                },
                {
                    "id": "ALL6901",
                    "value": "000000001"
                },
                {
                    "id": "ALL6970",
                    "value": "000000001"
                },
                {
                    "id": "ALL6971",
                    "value": "000000001"
                },
                {
                    "id": "ALL6980",
                    "value": "000000001"
                },
                {
                    "id": "ALL6981",
                    "value": "000000001"
                },
                {
                    "id": "ALL7110",
                    "value": "000000001"
                },
                {
                    "id": "ALL7111",
                    "value": "000000001"
                },
                {
                    "id": "ALL7113",
                    "value": "000000997"
                },
                {
                    "id": "ALL7115",
                    "value": "000000994"
                },
                {
                    "id": "ALL7116",
                    "value": "000000994"
                },
                {
                    "id": "ALL7120",
                    "value": "000000002"
                },
                {
                    "id": "ALL7130",
                    "value": "000000002"
                },
                {
                    "id": "ALL7140",
                    "value": "000000001"
                },
                {
                    "id": "ALL7170",
                    "value": "000000000"
                },
                {
                    "id": "ALL7310",
                    "value": "000000022"
                },
                {
                    "id": "ALL7311",
                    "value": "000000022"
                },
                {
                    "id": "ALL7312",
                    "value": "000000100"
                },
                {
                    "id": "ALL7320",
                    "value": "000000011"
                },
                {
                    "id": "ALL7330",
                    "value": "000000100"
                },
                {
                    "id": "ALL7331",
                    "value": "000000100"
                },
                {
                    "id": "ALL7332",
                    "value": "000000100"
                },
                {
                    "id": "ALL7333",
                    "value": "000000100"
                },
                {
                    "id": "ALL7334",
                    "value": "000000100"
                },
                {
                    "id": "ALL7336",
                    "value": "000000022"
                },
                {
                    "id": "ALL7337",
                    "value": "000000044"
                },
                {
                    "id": "ALL7338",
                    "value": "000000044"
                },
                {
                    "id": "ALL7340",
                    "value": "000000000"
                },
                {
                    "id": "ALL7346",
                    "value": "000000000"
                },
                {
                    "id": "ALL7347",
                    "value": "000000000"
                },
                {
                    "id": "ALL7348",
                    "value": "000000000"
                },
                {
                    "id": "ALL7350",
                    "value": "000000000"
                },
                {
                    "id": "ALL7358",
                    "value": "000000000"
                },
                {
                    "id": "ALL7360",
                    "value": "000000000"
                },
                {
                    "id": "ALL7370",
                    "value": "000000000"
                },
                {
                    "id": "ALL7440",
                    "value": "000000000"
                },
                {
                    "id": "ALL7442",
                    "value": "000000000"
                },
                {
                    "id": "ALL7443",
                    "value": "000000000"
                },
                {
                    "id": "ALL7444",
                    "value": "000000000"
                },
                {
                    "id": "ALL7450",
                    "value": "000000000"
                },
                {
                    "id": "ALL7454",
                    "value": "000000000"
                },
                {
                    "id": "ALL7460",
                    "value": "000000000"
                },
                {
                    "id": "ALL7470",
                    "value": "000000000"
                },
                {
                    "id": "ALL7516",
                    "value": "000000000"
                },
                {
                    "id": "ALL7517",
                    "value": "000000000"
                },
                {
                    "id": "ALL7518",
                    "value": "000000000"
                },
                {
                    "id": "ALL7519",
                    "value": "000000000"
                },
                {
                    "id": "ALL7936",
                    "value": "000000100"
                },
                {
                    "id": "ALL7937",
                    "value": "000000100"
                },
                {
                    "id": "ALL7938",
                    "value": "000000100"
                },
                {
                    "id": "ALL8020",
                    "value": "000001139"
                },
                {
                    "id": "ALL8026",
                    "value": "000999994"
                },
                {
                    "id": "ALL8107",
                    "value": "000009994"
                },
                {
                    "id": "ALL8110",
                    "value": "000000008"
                },
                {
                    "id": "ALL8120",
                    "value": "000000014"
                },
                {
                    "id": "ALL8121",
                    "value": "000000469"
                },
                {
                    "id": "ALL8122",
                    "value": "000000149"
                },
                {
                    "id": "ALL8123",
                    "value": "000009996"
                },
                {
                    "id": "ALL8150",
                    "value": "000009996"
                },
                {
                    "id": "ALL8151",
                    "value": "000009996"
                },
                {
                    "id": "ALL8152",
                    "value": "000009996"
                },
                {
                    "id": "ALL8153",
                    "value": "000009996"
                },
                {
                    "id": "ALL8154",
                    "value": "000009996"
                },
                {
                    "id": "ALL8155",
                    "value": "000009996"
                },
                {
                    "id": "ALL8157",
                    "value": "000009996"
                },
                {
                    "id": "ALL8158",
                    "value": "000009996"
                },
                {
                    "id": "ALL8160",
                    "value": "000009996"
                },
                {
                    "id": "ALL8162",
                    "value": "000009996"
                },
                {
                    "id": "ALL8163",
                    "value": "000009996"
                },
                {
                    "id": "ALL8164",
                    "value": "000009996"
                },
                {
                    "id": "ALL8167",
                    "value": "000009996"
                },
                {
                    "id": "ALL8170",
                    "value": "000000000"
                },
                {
                    "id": "ALL8171",
                    "value": "000000096"
                },
                {
                    "id": "ALL8172",
                    "value": "000000096"
                },
                {
                    "id": "ALL8183",
                    "value": "000009996"
                },
                {
                    "id": "ALL8220",
                    "value": "000000469"
                },
                {
                    "id": "ALL8221",
                    "value": "000000469"
                },
                {
                    "id": "ALL8222",
                    "value": "000000469"
                },
                {
                    "id": "ALL8223",
                    "value": "000009994"
                },
                {
                    "id": "ALL8225",
                    "value": "000009994"
                },
                {
                    "id": "ALL8250",
                    "value": "000009996"
                },
                {
                    "id": "ALL8253",
                    "value": "000009996"
                },
                {
                    "id": "ALL8257",
                    "value": "000009996"
                },
                {
                    "id": "ALL8259",
                    "value": "000009996"
                },
                {
                    "id": "ALL8270",
                    "value": "000000469"
                },
                {
                    "id": "ALL8271",
                    "value": "000000096"
                },
                {
                    "id": "ALL8272",
                    "value": "000000096"
                },
                {
                    "id": "ALL8320",
                    "value": "000000112"
                },
                {
                    "id": "ALL8321",
                    "value": "000000112"
                },
                {
                    "id": "ALL8323",
                    "value": "000009994"
                },
                {
                    "id": "ALL8325",
                    "value": "000009994"
                },
                {
                    "id": "ALL8351",
                    "value": "000009996"
                },
                {
                    "id": "ALL8352",
                    "value": "000009996"
                },
                {
                    "id": "ALL8353",
                    "value": "000009996"
                },
                {
                    "id": "ALL8354",
                    "value": "000009996"
                },
                {
                    "id": "ALL8355",
                    "value": "000009996"
                },
                {
                    "id": "ALL8358",
                    "value": "000009996"
                },
                {
                    "id": "ALL8370",
                    "value": "000000084"
                },
                {
                    "id": "ALL8423",
                    "value": "000000994"
                },
                {
                    "id": "ALL8425",
                    "value": "000000994"
                },
                {
                    "id": "ALL8426",
                    "value": "000000994"
                },
                {
                    "id": "ALL8552",
                    "value": "000009996"
                },
                {
                    "id": "ALL8555",
                    "value": "000009996"
                },
                {
                    "id": "ALL8558",
                    "value": "000009996"
                },
                {
                    "id": "ALL8560",
                    "value": "000009996"
                },
                {
                    "id": "ALL8723",
                    "value": "000000994"
                },
                {
                    "id": "ALL8725",
                    "value": "000000994"
                },
                {
                    "id": "ALL8726",
                    "value": "000000994"
                },
                {
                    "id": "ALL8800",
                    "value": "000000002"
                },
                {
                    "id": "ALL9110",
                    "value": "000000000"
                },
                {
                    "id": "ALL9118",
                    "value": "000000000"
                },
                {
                    "id": "ALL9120",
                    "value": "000000000"
                },
                {
                    "id": "ALL9121",
                    "value": "000000000"
                },
                {
                    "id": "ALL9122",
                    "value": "000000000"
                },
                {
                    "id": "ALL9123",
                    "value": "000000000"
                },
                {
                    "id": "ALL9124",
                    "value": "000000000"
                },
                {
                    "id": "ALL9125",
                    "value": "000000000"
                },
                {
                    "id": "ALL9128",
                    "value": "000000000"
                },
                {
                    "id": "ALL9130",
                    "value": "000000000"
                },
                {
                    "id": "ALL9134",
                    "value": "000000000"
                },
                {
                    "id": "ALL9135",
                    "value": "000000000"
                },
                {
                    "id": "ALL9138",
                    "value": "000000000"
                },
                {
                    "id": "ALL9139",
                    "value": "000000000"
                },
                {
                    "id": "ALL9140",
                    "value": "000000000"
                },
                {
                    "id": "ALL9141",
                    "value": "000000000"
                },
                {
                    "id": "ALL9144",
                    "value": "000000000"
                },
                {
                    "id": "ALL9145",
                    "value": "000000000"
                },
                {
                    "id": "ALL9148",
                    "value": "000000000"
                },
                {
                    "id": "ALL9149",
                    "value": "000000000"
                },
                {
                    "id": "ALL9150",
                    "value": "000000000"
                },
                {
                    "id": "ALL9160",
                    "value": "000000000"
                },
                {
                    "id": "ALL9171",
                    "value": "000000000"
                },
                {
                    "id": "ALL9177",
                    "value": "000000000"
                },
                {
                    "id": "ALL9178",
                    "value": "000000000"
                },
                {
                    "id": "ALL9180",
                    "value": "000000000"
                },
                {
                    "id": "ALL9187",
                    "value": "000000000"
                },
                {
                    "id": "ALL9188",
                    "value": "000000000"
                },
                {
                    "id": "ALL9189",
                    "value": "000000000"
                },
                {
                    "id": "ALL9210",
                    "value": "000009999"
                },
                {
                    "id": "ALL9211",
                    "value": "000009999"
                },
                {
                    "id": "ALL9219",
                    "value": "000009999"
                },
                {
                    "id": "ALL9220",
                    "value": "000009999"
                },
                {
                    "id": "ALL9221",
                    "value": "000009999"
                },
                {
                    "id": "ALL9222",
                    "value": "000009999"
                },
                {
                    "id": "ALL9223",
                    "value": "000009999"
                },
                {
                    "id": "ALL9226",
                    "value": "000009999"
                },
                {
                    "id": "ALL9229",
                    "value": "000009999"
                },
                {
                    "id": "ALL9230",
                    "value": "000009999"
                },
                {
                    "id": "ALL9239",
                    "value": "000009999"
                },
                {
                    "id": "ALL9240",
                    "value": "000009999"
                },
                {
                    "id": "ALL9249",
                    "value": "000009999"
                },
                {
                    "id": "ALL9250",
                    "value": "000009999"
                },
                {
                    "id": "ALL9260",
                    "value": "000009999"
                },
                {
                    "id": "ALL9280",
                    "value": "000009999"
                },
                {
                    "id": "ALL9330",
                    "value": "000000000"
                },
                {
                    "id": "ALL9340",
                    "value": "000000000"
                },
                {
                    "id": "ALL9380",
                    "value": "000000000"
                },
                {
                    "id": "ALL9950",
                    "value": "000000000"
                },
                {
                    "id": "ALL9951",
                    "value": "000000000"
                },
                {
                    "id": "ALM0201",
                    "value": "000000000"
                },
                {
                    "id": "ALM2001",
                    "value": "000000096"
                },
                {
                    "id": "ALM2002",
                    "value": "000000096"
                },
                {
                    "id": "ALM2320",
                    "value": "000000000"
                },
                {
                    "id": "ALM2329",
                    "value": "000000000"
                },
                {
                    "id": "ALM2350",
                    "value": "000000000"
                },
                {
                    "id": "ALM2359",
                    "value": "000000000"
                },
                {
                    "id": "ALM2380",
                    "value": "000000000"
                },
                {
                    "id": "ALM2389",
                    "value": "000000000"
                },
                {
                    "id": "ALM2390",
                    "value": "000000000"
                },
                {
                    "id": "ALM2399",
                    "value": "000000000"
                },
                {
                    "id": "ALM2700",
                    "value": "000000000"
                },
                {
                    "id": "ALM2709",
                    "value": "000000000"
                },
                {
                    "id": "ALM2720",
                    "value": "000000000"
                },
                {
                    "id": "ALM2729",
                    "value": "000000000"
                },
                {
                    "id": "ALM5072",
                    "value": "000000000"
                },
                {
                    "id": "ALM5074",
                    "value": "000000000"
                },
                {
                    "id": "ALM6160",
                    "value": "000000001"
                },
                {
                    "id": "ALM6169",
                    "value": "000000001"
                },
                {
                    "id": "ALM6200",
                    "value": "000000001"
                },
                {
                    "id": "ALM6209",
                    "value": "000000001"
                },
                {
                    "id": "ALM6270",
                    "value": "000000001"
                },
                {
                    "id": "ALM6279",
                    "value": "000000001"
                },
                {
                    "id": "ALM6280",
                    "value": "000000001"
                },
                {
                    "id": "ALM6289",
                    "value": "000000001"
                },
                {
                    "id": "ALS0000",
                    "value": "000000011"
                },
                {
                    "id": "ALS0001",
                    "value": "000000011"
                },
                {
                    "id": "ALS0337",
                    "value": "000000000"
                },
                {
                    "id": "ALS1300",
                    "value": "000000009"
                },
                {
                    "id": "ALS2000",
                    "value": "000000002"
                },
                {
                    "id": "ALS3215",
                    "value": "000000001"
                },
                {
                    "id": "ALS5400",
                    "value": "000020000"
                },
                {
                    "id": "ALS8220",
                    "value": "000000469"
                },
                {
                    "id": "ALX0436",
                    "value": "000000000"
                },
                {
                    "id": "ALX3510",
                    "value": "000000001"
                },
                {
                    "id": "ALX5020",
                    "value": "000000248"
                },
                {
                    "id": "ALX5030",
                    "value": "000000248"
                },
                {
                    "id": "ALX5039",
                    "value": "000000248"
                },
                {
                    "id": "ALX5830",
                    "value": "000000010"
                },
                {
                    "id": "ALX5839",
                    "value": "000000010"
                },
                {
                    "id": "ALX8220",
                    "value": "000000469"
                },
                {
                    "id": "AUA0300",
                    "value": "000000000"
                },
                {
                    "id": "AUA0416",
                    "value": "000000098"
                },
                {
                    "id": "AUA0426",
                    "value": "000000098"
                },
                {
                    "id": "AUA0436",
                    "value": "000000098"
                },
                {
                    "id": "AUA0437",
                    "value": "000000098"
                },
                {
                    "id": "AUA0438",
                    "value": "000000098"
                },
                {
                    "id": "AUA1300",
                    "value": "000000098"
                },
                {
                    "id": "AUA1305",
                    "value": "000000098"
                },
                {
                    "id": "AUA1380",
                    "value": "000000098"
                },
                {
                    "id": "AUA2320",
                    "value": "000000098"
                },
                {
                    "id": "AUA2328",
                    "value": "000000098"
                },
                {
                    "id": "AUA2350",
                    "value": "000000098"
                },
                {
                    "id": "AUA2358",
                    "value": "000000098"
                },
                {
                    "id": "AUA2388",
                    "value": "000000098"
                },
                {
                    "id": "AUA2800",
                    "value": "000000098"
                },
                {
                    "id": "AUA5020",
                    "value": "999999998"
                },
                {
                    "id": "AUA5320",
                    "value": "999999998"
                },
                {
                    "id": "AUA5400",
                    "value": "999999998"
                },
                {
                    "id": "AUA5420",
                    "value": "999999998"
                },
                {
                    "id": "AUA5421",
                    "value": "999999998"
                },
                {
                    "id": "AUA5520",
                    "value": "999999998"
                },
                {
                    "id": "AUA5820",
                    "value": "999999998"
                },
                {
                    "id": "AUA6160",
                    "value": "000000998"
                },
                {
                    "id": "AUA6200",
                    "value": "000000998"
                },
                {
                    "id": "AUA6280",
                    "value": "000000998"
                },
                {
                    "id": "AUA7201",
                    "value": "000000998"
                },
                {
                    "id": "AUA8120",
                    "value": "000009998"
                },
                {
                    "id": "AUA8122",
                    "value": "000009998"
                },
                {
                    "id": "AUA8151",
                    "value": "000009998"
                },
                {
                    "id": "AUA8220",
                    "value": "000009998"
                },
                {
                    "id": "AUA8320",
                    "value": "000009998"
                },
                {
                    "id": "AUA8370",
                    "value": "000009998"
                },
                {
                    "id": "AUA8811",
                    "value": "000009998"
                },
                {
                    "id": "AUA8820",
                    "value": "000009998"
                },
                {
                    "id": "AUL0300",
                    "value": "000000000"
                },
                {
                    "id": "AUL0416",
                    "value": "000000098"
                },
                {
                    "id": "AUL0436",
                    "value": "000000098"
                },
                {
                    "id": "AUL0700",
                    "value": "000000098"
                },
                {
                    "id": "AUL2000",
                    "value": "000000098"
                },
                {
                    "id": "AUL4180",
                    "value": "000000998"
                },
                {
                    "id": "AUL5020",
                    "value": "999999998"
                },
                {
                    "id": "AUL5120",
                    "value": "999999998"
                },
                {
                    "id": "AUL5122",
                    "value": "999999998"
                },
                {
                    "id": "AUL5320",
                    "value": "999999998"
                },
                {
                    "id": "AUL5420",
                    "value": "999999998"
                },
                {
                    "id": "AUL5820",
                    "value": "999999998"
                },
                {
                    "id": "AUL8120",
                    "value": "000009998"
                },
                {
                    "id": "AUL8122",
                    "value": "000009998"
                },
                {
                    "id": "AUL8132",
                    "value": "000099998"
                },
                {
                    "id": "AUL8140",
                    "value": "000009998"
                },
                {
                    "id": "AUL8220",
                    "value": "000009998"
                },
                {
                    "id": "AUL8222",
                    "value": "000009998"
                },
                {
                    "id": "AUT0300",
                    "value": "000000000"
                },
                {
                    "id": "AUT0416",
                    "value": "000000098"
                },
                {
                    "id": "AUT0436",
                    "value": "000000098"
                },
                {
                    "id": "AUT0700",
                    "value": "000000098"
                },
                {
                    "id": "AUT2000",
                    "value": "000000098"
                },
                {
                    "id": "AUT4180",
                    "value": "000000998"
                },
                {
                    "id": "AUT5020",
                    "value": "999999998"
                },
                {
                    "id": "AUT5120",
                    "value": "999999998"
                },
                {
                    "id": "AUT5122",
                    "value": "999999998"
                },
                {
                    "id": "AUT5238",
                    "value": "999999998"
                },
                {
                    "id": "AUT5320",
                    "value": "999999998"
                },
                {
                    "id": "AUT5420",
                    "value": "999999998"
                },
                {
                    "id": "AUT5620",
                    "value": "999999998"
                },
                {
                    "id": "AUT5820",
                    "value": "999999998"
                },
                {
                    "id": "AUT5838",
                    "value": "999999998"
                },
                {
                    "id": "AUT5923",
                    "value": "999999998"
                },
                {
                    "id": "AUT5926",
                    "value": "999999998"
                },
                {
                    "id": "AUT5930",
                    "value": "999999998"
                },
                {
                    "id": "AUT7110",
                    "value": "000000998"
                },
                {
                    "id": "AUT8120",
                    "value": "000009998"
                },
                {
                    "id": "AUT8122",
                    "value": "000009998"
                },
                {
                    "id": "AUT8132",
                    "value": "000099998"
                },
                {
                    "id": "AUT8140",
                    "value": "000009998"
                },
                {
                    "id": "AUT8220",
                    "value": "000009998"
                },
                {
                    "id": "AUT8222",
                    "value": "000009998"
                },
                {
                    "id": "BAX0416",
                    "value": "000000001"
                },
                {
                    "id": "BAX0436",
                    "value": "000000000"
                },
                {
                    "id": "BAX3510",
                    "value": "000000001"
                },
                {
                    "id": "BAX5020",
                    "value": "000000248"
                },
                {
                    "id": "BAX5030",
                    "value": "000000248"
                },
                {
                    "id": "BCA0300",
                    "value": "000000002"
                },
                {
                    "id": "BCA0400",
                    "value": "000000001"
                },
                {
                    "id": "BCA0401",
                    "value": "000000098"
                },
                {
                    "id": "BCA0416",
                    "value": "000000001"
                },
                {
                    "id": "BCA0436",
                    "value": "000000000"
                },
                {
                    "id": "BCA1300",
                    "value": "000000002"
                },
                {
                    "id": "BCA2350",
                    "value": "000000000"
                },
                {
                    "id": "BCA2358",
                    "value": "000000000"
                },
                {
                    "id": "BCA2380",
                    "value": "000000000"
                },
                {
                    "id": "BCA2388",
                    "value": "000000000"
                },
                {
                    "id": "BCA3510",
                    "value": "000000001"
                },
                {
                    "id": "BCA3511",
                    "value": "000000000"
                },
                {
                    "id": "BCA4180",
                    "value": "000000000"
                },
                {
                    "id": "BCA5020",
                    "value": "000000248"
                },
                {
                    "id": "BCA5021",
                    "value": "999999998"
                },
                {
                    "id": "BCA5028",
                    "value": "000000248"
                },
                {
                    "id": "BCA5030",
                    "value": "000000248"
                },
                {
                    "id": "BCA5040",
                    "value": "000000000"
                },
                {
                    "id": "BCA5043",
                    "value": "000000000"
                },
                {
                    "id": "BCA5047",
                    "value": "000000000"
                },
                {
                    "id": "BCA5070",
                    "value": "000000000"
                },
                {
                    "id": "BCA5121",
                    "value": "000000248"
                },
                {
                    "id": "BCA5122",
                    "value": "000000248"
                },
                {
                    "id": "BCA5130",
                    "value": "000000248"
                },
                {
                    "id": "BCA5137",
                    "value": "000000248"
                },
                {
                    "id": "BCA5138",
                    "value": "000000248"
                },
                {
                    "id": "BCA5430",
                    "value": "000013500"
                },
                {
                    "id": "BCA5431",
                    "value": "000013500"
                },
                {
                    "id": "BCA5740",
                    "value": "000000000"
                },
                {
                    "id": "BCA5743",
                    "value": "000000000"
                },
                {
                    "id": "BCA5747",
                    "value": "000000000"
                },
                {
                    "id": "BCA5770",
                    "value": "000000000"
                },
                {
                    "id": "BCA6200",
                    "value": "000000001"
                },
                {
                    "id": "BCA6201",
                    "value": "000000998"
                },
                {
                    "id": "BCA6204",
                    "value": "000000001"
                },
                {
                    "id": "BCA6210",
                    "value": "000000994"
                },
                {
                    "id": "BCA6220",
                    "value": "000000994"
                },
                {
                    "id": "BCA6280",
                    "value": "000000001"
                },
                {
                    "id": "BCA7211",
                    "value": "000000994"
                },
                {
                    "id": "BCA7212",
                    "value": "000000994"
                },
                {
                    "id": "BCA7213",
                    "value": "000000994"
                },
                {
                    "id": "BCA7214",
                    "value": "000000994"
                },
                {
                    "id": "BCA7300",
                    "value": "000000022"
                },
                {
                    "id": "BCA7600",
                    "value": "000000100"
                },
                {
                    "id": "BCA8110",
                    "value": "000000037"
                },
                {
                    "id": "BCA8120",
                    "value": "000000277"
                },
                {
                    "id": "BCA8122",
                    "value": "000000469"
                },
                {
                    "id": "BCA8150",
                    "value": "000009996"
                },
                {
                    "id": "BCA8151",
                    "value": "000009996"
                },
                {
                    "id": "BCA8153",
                    "value": "000009996"
                },
                {
                    "id": "BCA8155",
                    "value": "000009996"
                },
                {
                    "id": "BCA8157",
                    "value": "000009996"
                },
                {
                    "id": "BCA8160",
                    "value": "000009996"
                },
                {
                    "id": "BCA8220",
                    "value": "000000469"
                },
                {
                    "id": "BCA8222",
                    "value": "000000469"
                },
                {
                    "id": "BCA8320",
                    "value": "000000373"
                },
                {
                    "id": "BCA8370",
                    "value": "000000355"
                },
                {
                    "id": "BCC0300",
                    "value": "000000002"
                },
                {
                    "id": "BCC0316",
                    "value": "000000001"
                },
                {
                    "id": "BCC0317",
                    "value": "000000001"
                },
                {
                    "id": "BCC0318",
                    "value": "000000001"
                },
                {
                    "id": "BCC0400",
                    "value": "000000001"
                },
                {
                    "id": "BCC0416",
                    "value": "000000001"
                },
                {
                    "id": "BCC0436",
                    "value": "000000000"
                },
                {
                    "id": "BCC0437",
                    "value": "000000000"
                },
                {
                    "id": "BCC0438",
                    "value": "000000000"
                },
                {
                    "id": "BCC0446",
                    "value": "000000001"
                },
                {
                    "id": "BCC0700",
                    "value": "000000001"
                },
                {
                    "id": "BCC1300",
                    "value": "000000002"
                },
                {
                    "id": "BCC1360",
                    "value": "000000001"
                },
                {
                    "id": "BCC1370",
                    "value": "000000001"
                },
                {
                    "id": "BCC1380",
                    "value": "000000001"
                },
                {
                    "id": "BCC1401",
                    "value": "000000001"
                },
                {
                    "id": "BCC2000",
                    "value": "000000001"
                },
                {
                    "id": "BCC2306",
                    "value": "000000000"
                },
                {
                    "id": "BCC2307",
                    "value": "000000000"
                },
                {
                    "id": "BCC2308",
                    "value": "000000000"
                },
                {
                    "id": "BCC2320",
                    "value": "000000000"
                },
                {
                    "id": "BCC2326",
                    "value": "000000000"
                },
                {
                    "id": "BCC2327",
                    "value": "000000000"
                },
                {
                    "id": "BCC2328",
                    "value": "000000000"
                },
                {
                    "id": "BCC2336",
                    "value": "000000000"
                },
                {
                    "id": "BCC2337",
                    "value": "000000000"
                },
                {
                    "id": "BCC2338",
                    "value": "000000000"
                },
                {
                    "id": "BCC2350",
                    "value": "000000000"
                },
                {
                    "id": "BCC2356",
                    "value": "000000000"
                },
                {
                    "id": "BCC2357",
                    "value": "000000000"
                },
                {
                    "id": "BCC2358",
                    "value": "000000000"
                },
                {
                    "id": "BCC2366",
                    "value": "000000000"
                },
                {
                    "id": "BCC2367",
                    "value": "000000000"
                },
                {
                    "id": "BCC2368",
                    "value": "000000000"
                },
                {
                    "id": "BCC2380",
                    "value": "000000000"
                },
                {
                    "id": "BCC2386",
                    "value": "000000000"
                },
                {
                    "id": "BCC2387",
                    "value": "000000000"
                },
                {
                    "id": "BCC2388",
                    "value": "000000000"
                },
                {
                    "id": "BCC2391",
                    "value": "000000000"
                },
                {
                    "id": "BCC2607",
                    "value": "000000000"
                },
                {
                    "id": "BCC2687",
                    "value": "000000000"
                },
                {
                    "id": "BCC2688",
                    "value": "000000000"
                },
                {
                    "id": "BCC2800",
                    "value": "000000000"
                },
                {
                    "id": "BCC2900",
                    "value": "000000000"
                },
                {
                    "id": "BCC2930",
                    "value": "000000000"
                },
                {
                    "id": "BCC3110",
                    "value": "000000001"
                },
                {
                    "id": "BCC3341",
                    "value": "000000001"
                },
                {
                    "id": "BCC3342",
                    "value": "000000001"
                },
                {
                    "id": "BCC3343",
                    "value": "000000001"
                },
                {
                    "id": "BCC3344",
                    "value": "000000001"
                },
                {
                    "id": "BCC3345",
                    "value": "000000001"
                },
                {
                    "id": "BCC3346",
                    "value": "000000001"
                },
                {
                    "id": "BCC3410",
                    "value": "000000001"
                },
                {
                    "id": "BCC3421",
                    "value": "000000000"
                },
                {
                    "id": "BCC3422",
                    "value": "000000000"
                },
                {
                    "id": "BCC3423",
                    "value": "000000000"
                },
                {
                    "id": "BCC3424",
                    "value": "000000000"
                },
                {
                    "id": "BCC3448",
                    "value": "000000001"
                },
                {
                    "id": "BCC3449",
                    "value": "000000001"
                },
                {
                    "id": "BCC3456",
                    "value": "000000001"
                },
                {
                    "id": "BCC3476",
                    "value": "000000000"
                },
                {
                    "id": "BCC3480",
                    "value": "000000001"
                },
                {
                    "id": "BCC3485",
                    "value": "000000000"
                },
                {
                    "id": "BCC3510",
                    "value": "000000001"
                },
                {
                    "id": "BCC3511",
                    "value": "000000000"
                },
                {
                    "id": "BCC3512",
                    "value": "000000000"
                },
                {
                    "id": "BCC3515",
                    "value": "000000000"
                },
                {
                    "id": "BCC4070",
                    "value": "000000000"
                },
                {
                    "id": "BCC4080",
                    "value": "000000000"
                },
                {
                    "id": "BCC4380",
                    "value": "000000000"
                },
                {
                    "id": "BCC4780",
                    "value": "000000000"
                },
                {
                    "id": "BCC5020",
                    "value": "000000248"
                },
                {
                    "id": "BCC5030",
                    "value": "000000248"
                },
                {
                    "id": "BCC5038",
                    "value": "000000248"
                },
                {
                    "id": "BCC5120",
                    "value": "000000248"
                },
                {
                    "id": "BCC5122",
                    "value": "000000248"
                },
                {
                    "id": "BCC5227",
                    "value": "000000155"
                },
                {
                    "id": "BCC5228",
                    "value": "000000155"
                },
                {
                    "id": "BCC5238",
                    "value": "000000145"
                },
                {
                    "id": "BCC5239",
                    "value": "000000093"
                },
                {
                    "id": "BCC5320",
                    "value": "000013500"
                },
                {
                    "id": "BCC5400",
                    "value": "000013500"
                },
                {
                    "id": "BCC5420",
                    "value": "000013500"
                },
                {
                    "id": "BCC5421",
                    "value": "000013500"
                },
                {
                    "id": "BCC5422",
                    "value": "000013500"
                },
                {
                    "id": "BCC5423",
                    "value": "000013500"
                },
                {
                    "id": "BCC5427",
                    "value": "000013500"
                },
                {
                    "id": "BCC5520",
                    "value": "000013500"
                },
                {
                    "id": "BCC5620",
                    "value": "000013252"
                },
                {
                    "id": "BCC5627",
                    "value": "000013252"
                },
                {
                    "id": "BCC5820",
                    "value": "000000010"
                },
                {
                    "id": "BCC5830",
                    "value": "000000010"
                },
                {
                    "id": "BCC5838",
                    "value": "000000010"
                },
                {
                    "id": "BCC5920",
                    "value": "000000155"
                },
                {
                    "id": "BCC5930",
                    "value": "000000155"
                },
                {
                    "id": "BCC6160",
                    "value": "000000001"
                },
                {
                    "id": "BCC6200",
                    "value": "000000001"
                },
                {
                    "id": "BCC6280",
                    "value": "000000001"
                },
                {
                    "id": "BCC7110",
                    "value": "000000002"
                },
                {
                    "id": "BCC7117",
                    "value": "000000994"
                },
                {
                    "id": "BCC7120",
                    "value": "000000002"
                },
                {
                    "id": "BCC7130",
                    "value": "000000002"
                },
                {
                    "id": "BCC7140",
                    "value": "000000002"
                },
                {
                    "id": "BCC7147",
                    "value": "000000994"
                },
                {
                    "id": "BCC7150",
                    "value": "000000006"
                },
                {
                    "id": "BCC7160",
                    "value": "000000063"
                },
                {
                    "id": "BCC7200",
                    "value": "000000100"
                },
                {
                    "id": "BCC7216",
                    "value": "000000100"
                },
                {
                    "id": "BCC7228",
                    "value": "000000994"
                },
                {
                    "id": "BCC7433",
                    "value": "000000100"
                },
                {
                    "id": "BCC7437",
                    "value": "000000050"
                },
                {
                    "id": "BCC7440",
                    "value": "000000000"
                },
                {
                    "id": "BCC7450",
                    "value": "000000000"
                },
                {
                    "id": "BCC7460",
                    "value": "000000000"
                },
                {
                    "id": "BCC7470",
                    "value": "000000000"
                },
                {
                    "id": "BCC7481",
                    "value": "000000000"
                },
                {
                    "id": "BCC7482",
                    "value": "000000000"
                },
                {
                    "id": "BCC7483",
                    "value": "000000000"
                },
                {
                    "id": "BCC7516",
                    "value": "000000000"
                },
                {
                    "id": "BCC7517",
                    "value": "000000000"
                },
                {
                    "id": "BCC7518",
                    "value": "000000000"
                },
                {
                    "id": "BCC7592",
                    "value": "000000000"
                },
                {
                    "id": "BCC7610",
                    "value": "000000100"
                },
                {
                    "id": "BCC7620",
                    "value": "000000100"
                },
                {
                    "id": "BCC7700",
                    "value": "000000098"
                },
                {
                    "id": "BCC7707",
                    "value": "000000100"
                },
                {
                    "id": "BCC7708",
                    "value": "000000100"
                },
                {
                    "id": "BCC7800",
                    "value": "000001325"
                },
                {
                    "id": "BCC7801",
                    "value": "999999994"
                },
                {
                    "id": "BCC7910",
                    "value": "000000100"
                },
                {
                    "id": "BCC7911",
                    "value": "000000100"
                },
                {
                    "id": "BCC8120",
                    "value": "000000277"
                },
                {
                    "id": "BCC8122",
                    "value": "000000469"
                },
                {
                    "id": "BCC8132",
                    "value": "000014297"
                },
                {
                    "id": "BCC8220",
                    "value": "000000469"
                },
                {
                    "id": "BCC8222",
                    "value": "000000469"
                },
                {
                    "id": "BCC8320",
                    "value": "000000373"
                },
                {
                    "id": "BCC8322",
                    "value": "000000469"
                },
                {
                    "id": "BCC8337",
                    "value": "000000011"
                },
                {
                    "id": "BCC8338",
                    "value": "000000023"
                },
                {
                    "id": "BCN0300",
                    "value": "000000000"
                },
                {
                    "id": "BCN3480",
                    "value": "000000098"
                },
                {
                    "id": "BCN3485",
                    "value": "000000098"
                },
                {
                    "id": "BCN5020",
                    "value": "999999998"
                },
                {
                    "id": "BCN5030",
                    "value": "999999998"
                },
                {
                    "id": "BCN5038",
                    "value": "999999998"
                },
                {
                    "id": "BCN5238",
                    "value": "999999998"
                },
                {
                    "id": "BCN5239",
                    "value": "999999998"
                },
                {
                    "id": "BCN5838",
                    "value": "999999998"
                },
                {
                    "id": "BCN5930",
                    "value": "999999998"
                },
                {
                    "id": "BCN7150",
                    "value": "000000998"
                },
                {
                    "id": "BCN7160",
                    "value": "000000998"
                },
                {
                    "id": "BCN7592",
                    "value": "000000998"
                },
                {
                    "id": "BCN8120",
                    "value": "000009998"
                },
                {
                    "id": "BCN8220",
                    "value": "000009998"
                },
                {
                    "id": "BCX0416",
                    "value": "000000001"
                },
                {
                    "id": "BCX0436",
                    "value": "000000000"
                },
                {
                    "id": "BCX0438",
                    "value": "000000000"
                },
                {
                    "id": "BCX1300",
                    "value": "000000002"
                },
                {
                    "id": "BCX3421",
                    "value": "000000000"
                },
                {
                    "id": "BCX3422",
                    "value": "000000000"
                },
                {
                    "id": "BCX3423",
                    "value": "000000000"
                },
                {
                    "id": "BCX3510",
                    "value": "000000001"
                },
                {
                    "id": "BCX5020",
                    "value": "000000248"
                },
                {
                    "id": "BCX5030",
                    "value": "000000248"
                },
                {
                    "id": "BCX5320",
                    "value": "000013500"
                },
                {
                    "id": "BCX5420",
                    "value": "000013500"
                },
                {
                    "id": "BCX5830",
                    "value": "000000010"
                },
                {
                    "id": "BCX7110",
                    "value": "000000002"
                },
                {
                    "id": "BRC0300",
                    "value": "000000003"
                },
                {
                    "id": "BRC0400",
                    "value": "000000002"
                },
                {
                    "id": "BRC0416",
                    "value": "000000002"
                },
                {
                    "id": "BRC0437",
                    "value": "000000000"
                },
                {
                    "id": "BRC1300",
                    "value": "000000003"
                },
                {
                    "id": "BRC2000",
                    "value": "000000002"
                },
                {
                    "id": "BRC2328",
                    "value": "000000000"
                },
                {
                    "id": "BRC2358",
                    "value": "000000000"
                },
                {
                    "id": "BRC2388",
                    "value": "000000000"
                },
                {
                    "id": "BRC2800",
                    "value": "000000000"
                },
                {
                    "id": "BRC3425",
                    "value": "000000000"
                },
                {
                    "id": "BRC3480",
                    "value": "000000001"
                },
                {
                    "id": "BRC3481",
                    "value": "000000000"
                },
                {
                    "id": "BRC3485",
                    "value": "000000000"
                },
                {
                    "id": "BRC3510",
                    "value": "000000001"
                },
                {
                    "id": "BRC4180",
                    "value": "000000000"
                },
                {
                    "id": "BRC5020",
                    "value": "000000248"
                },
                {
                    "id": "BRC5030",
                    "value": "000000248"
                },
                {
                    "id": "BRC5038",
                    "value": "000000248"
                },
                {
                    "id": "BRC5220",
                    "value": "000000155"
                },
                {
                    "id": "BRC5238",
                    "value": "000000145"
                },
                {
                    "id": "BRC5239",
                    "value": "000000093"
                },
                {
                    "id": "BRC5320",
                    "value": "000021000"
                },
                {
                    "id": "BRC5620",
                    "value": "000020752"
                },
                {
                    "id": "BRC5747",
                    "value": "000000000"
                },
                {
                    "id": "BRC5830",
                    "value": "000000010"
                },
                {
                    "id": "BRC5838",
                    "value": "000000010"
                },
                {
                    "id": "BRC5930",
                    "value": "000000155"
                },
                {
                    "id": "BRC6160",
                    "value": "000000001"
                },
                {
                    "id": "BRC6200",
                    "value": "000000001"
                },
                {
                    "id": "BRC6280",
                    "value": "000000001"
                },
                {
                    "id": "BRC7140",
                    "value": "000000001"
                },
                {
                    "id": "BRC7150",
                    "value": "000000006"
                },
                {
                    "id": "BRC7160",
                    "value": "000000063"
                },
                {
                    "id": "BRC7180",
                    "value": "000000006"
                },
                {
                    "id": "BRC7190",
                    "value": "000000063"
                },
                {
                    "id": "BRC7517",
                    "value": "000000000"
                },
                {
                    "id": "BRC7591",
                    "value": "000000000"
                },
                {
                    "id": "BRC7592",
                    "value": "000000000"
                },
                {
                    "id": "BRC8120",
                    "value": "000000149"
                },
                {
                    "id": "BRC8151",
                    "value": "000009996"
                },
                {
                    "id": "BRC8158",
                    "value": "000009996"
                },
                {
                    "id": "BRC8220",
                    "value": "000000469"
                },
                {
                    "id": "BRC8320",
                    "value": "000000298"
                },
                {
                    "id": "BUS0416",
                    "value": "000000098"
                },
                {
                    "id": "BUS5020",
                    "value": "999999998"
                },
                {
                    "id": "BUS5120",
                    "value": "999999998"
                },
                {
                    "id": "BUS7110",
                    "value": "000000998"
                },
                {
                    "id": "BUS7130",
                    "value": "000000998"
                },
                {
                    "id": "BUS8122",
                    "value": "000009998"
                },
                {
                    "id": "BUS8132",
                    "value": "000099998"
                },
                {
                    "id": "COL2740",
                    "value": "000000000"
                },
                {
                    "id": "COL2750",
                    "value": "000000000"
                },
                {
                    "id": "COL2757",
                    "value": "000000000"
                },
                {
                    "id": "COL2758",
                    "value": "000000000"
                },
                {
                    "id": "COL2760",
                    "value": "000000000"
                },
                {
                    "id": "COL2762",
                    "value": "000000000"
                },
                {
                    "id": "COL2763",
                    "value": "000000000"
                },
                {
                    "id": "COL2764",
                    "value": "000000000"
                },
                {
                    "id": "COL2766",
                    "value": "000000000"
                },
                {
                    "id": "COL2767",
                    "value": "000000000"
                },
                {
                    "id": "COL2768",
                    "value": "000000000"
                },
                {
                    "id": "COL2770",
                    "value": "000000000"
                },
                {
                    "id": "COL2780",
                    "value": "000000000"
                },
                {
                    "id": "COL2790",
                    "value": "000000000"
                },
                {
                    "id": "COL3200",
                    "value": "000000000"
                },
                {
                    "id": "COL3201",
                    "value": "000000000"
                },
                {
                    "id": "COL3203",
                    "value": "000000000"
                },
                {
                    "id": "COL3204",
                    "value": "000000000"
                },
                {
                    "id": "COL3206",
                    "value": "000000000"
                },
                {
                    "id": "COL3207",
                    "value": "000000000"
                },
                {
                    "id": "COL3208",
                    "value": "000000000"
                },
                {
                    "id": "COL3209",
                    "value": "000000000"
                },
                {
                    "id": "COL3210",
                    "value": "000000000"
                },
                {
                    "id": "COL3211",
                    "value": "000000000"
                },
                {
                    "id": "COL3212",
                    "value": "000000000"
                },
                {
                    "id": "COL3217",
                    "value": "000000000"
                },
                {
                    "id": "COL3218",
                    "value": "000000000"
                },
                {
                    "id": "COL3219",
                    "value": "000000000"
                },
                {
                    "id": "COL3230",
                    "value": "000000000"
                },
                {
                    "id": "COL3236",
                    "value": "000000000"
                },
                {
                    "id": "COL3237",
                    "value": "000000000"
                },
                {
                    "id": "COL3238",
                    "value": "000000000"
                },
                {
                    "id": "COL3240",
                    "value": "000000000"
                },
                {
                    "id": "COL3242",
                    "value": "000000000"
                },
                {
                    "id": "COL3243",
                    "value": "000000000"
                },
                {
                    "id": "COL3244",
                    "value": "000000000"
                },
                {
                    "id": "COL5060",
                    "value": "000000000"
                },
                {
                    "id": "COL5062",
                    "value": "000000000"
                },
                {
                    "id": "COL5063",
                    "value": "000000000"
                },
                {
                    "id": "COL5064",
                    "value": "000000000"
                },
                {
                    "id": "COL5066",
                    "value": "000000000"
                },
                {
                    "id": "COL5067",
                    "value": "000000000"
                },
                {
                    "id": "COL5068",
                    "value": "000000000"
                },
                {
                    "id": "COL5069",
                    "value": "000000000"
                },
                {
                    "id": "COL8165",
                    "value": "000009998"
                },
                {
                    "id": "COL8168",
                    "value": "000009998"
                },
                {
                    "id": "COL8190",
                    "value": "000009998"
                },
                {
                    "id": "COL8191",
                    "value": "000009998"
                },
                {
                    "id": "COL8192",
                    "value": "000009998"
                },
                {
                    "id": "COL8193",
                    "value": "000009998"
                },
                {
                    "id": "COL8194",
                    "value": "000009998"
                },
                {
                    "id": "COL8195",
                    "value": "000009998"
                },
                {
                    "id": "COL8196",
                    "value": "000009998"
                },
                {
                    "id": "COL8197",
                    "value": "000009998"
                },
                {
                    "id": "COL8198",
                    "value": "000009998"
                },
                {
                    "id": "COL8199",
                    "value": "000009998"
                },
                {
                    "id": "CRU0300",
                    "value": "000000000"
                },
                {
                    "id": "CRU0416",
                    "value": "000000098"
                },
                {
                    "id": "CRU0436",
                    "value": "000000098"
                },
                {
                    "id": "CRU1300",
                    "value": "000000098"
                },
                {
                    "id": "CRU2388",
                    "value": "000000098"
                },
                {
                    "id": "CRU4180",
                    "value": "000000998"
                },
                {
                    "id": "CRU5030",
                    "value": "999999998"
                },
                {
                    "id": "CRU5320",
                    "value": "999999998"
                },
                {
                    "id": "CRU6200",
                    "value": "000000998"
                },
                {
                    "id": "CRU6280",
                    "value": "000000998"
                },
                {
                    "id": "CRU8151",
                    "value": "000009998"
                },
                {
                    "id": "CRU8320",
                    "value": "000009998"
                },
                {
                    "id": "FIP0300",
                    "value": "000000000"
                },
                {
                    "id": "FIP0416",
                    "value": "000000098"
                },
                {
                    "id": "FIP0436",
                    "value": "000000098"
                },
                {
                    "id": "FIP0437",
                    "value": "000000098"
                },
                {
                    "id": "FIP0438",
                    "value": "000000098"
                },
                {
                    "id": "FIP1300",
                    "value": "000000098"
                },
                {
                    "id": "FIP1380",
                    "value": "000000098"
                },
                {
                    "id": "FIP2000",
                    "value": "000000098"
                },
                {
                    "id": "FIP2320",
                    "value": "000000098"
                },
                {
                    "id": "FIP2328",
                    "value": "000000098"
                },
                {
                    "id": "FIP2350",
                    "value": "000000098"
                },
                {
                    "id": "FIP2358",
                    "value": "000000098"
                },
                {
                    "id": "FIP2380",
                    "value": "000000098"
                },
                {
                    "id": "FIP2388",
                    "value": "000000098"
                },
                {
                    "id": "FIP2800",
                    "value": "000000098"
                },
                {
                    "id": "FIP5020",
                    "value": "999999998"
                },
                {
                    "id": "FIP5120",
                    "value": "999999998"
                },
                {
                    "id": "FIP5320",
                    "value": "999999998"
                },
                {
                    "id": "FIP5420",
                    "value": "999999998"
                },
                {
                    "id": "FIP5520",
                    "value": "999999998"
                },
                {
                    "id": "FIP5820",
                    "value": "999999998"
                },
                {
                    "id": "FIP6200",
                    "value": "000000998"
                },
                {
                    "id": "FIP6280",
                    "value": "000000998"
                },
                {
                    "id": "FIP8120",
                    "value": "000009998"
                },
                {
                    "id": "FIP8220",
                    "value": "000009998"
                },
                {
                    "id": "FIP8320",
                    "value": "000009998"
                },
                {
                    "id": "GLBDECS",
                    "value": "000000000"
                },
                {
                    "id": "HLC0300",
                    "value": "000000000"
                },
                {
                    "id": "HLC0401",
                    "value": "000000098"
                },
                {
                    "id": "HLC0402",
                    "value": "000000098"
                },
                {
                    "id": "HLC0416",
                    "value": "000000098"
                },
                {
                    "id": "HLC0436",
                    "value": "000000098"
                },
                {
                    "id": "HLC0437",
                    "value": "000000098"
                },
                {
                    "id": "HLC0438",
                    "value": "000000098"
                },
                {
                    "id": "HLC0700",
                    "value": "000000098"
                },
                {
                    "id": "HLC1402",
                    "value": "000000098"
                },
                {
                    "id": "HLC2000",
                    "value": "000000098"
                },
                {
                    "id": "HLC2156",
                    "value": "000000098"
                },
                {
                    "id": "HLC2320",
                    "value": "000000098"
                },
                {
                    "id": "HLC2328",
                    "value": "000000098"
                },
                {
                    "id": "HLC2358",
                    "value": "000000098"
                },
                {
                    "id": "HLC2388",
                    "value": "000000098"
                },
                {
                    "id": "HLC3410",
                    "value": "000000098"
                },
                {
                    "id": "HLC3482",
                    "value": "000000098"
                },
                {
                    "id": "HLC3483",
                    "value": "000000098"
                },
                {
                    "id": "HLC3485",
                    "value": "000000098"
                },
                {
                    "id": "HLC5020",
                    "value": "999999998"
                },
                {
                    "id": "HLC5021",
                    "value": "999999998"
                },
                {
                    "id": "HLC5030",
                    "value": "999999998"
                },
                {
                    "id": "HLC5120",
                    "value": "999999998"
                },
                {
                    "id": "HLC5122",
                    "value": "999999998"
                },
                {
                    "id": "HLC5220",
                    "value": "999999998"
                },
                {
                    "id": "HLC5238",
                    "value": "999999998"
                },
                {
                    "id": "HLC5320",
                    "value": "999999998"
                },
                {
                    "id": "HLC5420",
                    "value": "999999998"
                },
                {
                    "id": "HLC5422",
                    "value": "999999998"
                },
                {
                    "id": "HLC5520",
                    "value": "999999998"
                },
                {
                    "id": "HLC5620",
                    "value": "999999998"
                },
                {
                    "id": "HLC5820",
                    "value": "999999998"
                },
                {
                    "id": "HLC5830",
                    "value": "999999998"
                },
                {
                    "id": "HLC5838",
                    "value": "999999998"
                },
                {
                    "id": "HLC5930",
                    "value": "999999998"
                },
                {
                    "id": "HLC6201",
                    "value": "000000998"
                },
                {
                    "id": "HLC7110",
                    "value": "000000998"
                },
                {
                    "id": "HLC7117",
                    "value": "000000998"
                },
                {
                    "id": "HLC7130",
                    "value": "000000998"
                },
                {
                    "id": "HLC7150",
                    "value": "000000998"
                },
                {
                    "id": "HLC7180",
                    "value": "000000998"
                },
                {
                    "id": "HLC8120",
                    "value": "000009998"
                },
                {
                    "id": "HLC8122",
                    "value": "000009998"
                },
                {
                    "id": "HLC8132",
                    "value": "000099998"
                },
                {
                    "id": "HLC8220",
                    "value": "000009998"
                },
                {
                    "id": "HLC8222",
                    "value": "000009998"
                },
                {
                    "id": "HLC8320",
                    "value": "000009998"
                },
                {
                    "id": "ILJ0300",
                    "value": "000000000"
                },
                {
                    "id": "ILJ0316",
                    "value": "000000098"
                },
                {
                    "id": "ILJ0416",
                    "value": "000000098"
                },
                {
                    "id": "ILJ5030",
                    "value": "999999998"
                },
                {
                    "id": "ILJ5320",
                    "value": "999999998"
                },
                {
                    "id": "ILJ5820",
                    "value": "999999998"
                },
                {
                    "id": "ILJ8120",
                    "value": "000009998"
                },
                {
                    "id": "ILJ8220",
                    "value": "000009998"
                },
                {
                    "id": "ILN0300",
                    "value": "000000006"
                },
                {
                    "id": "ILN0316",
                    "value": "000000000"
                },
                {
                    "id": "ILN0317",
                    "value": "000000002"
                },
                {
                    "id": "ILN0318",
                    "value": "000000002"
                },
                {
                    "id": "ILN0403",
                    "value": "000000098"
                },
                {
                    "id": "ILN0416",
                    "value": "000000000"
                },
                {
                    "id": "ILN0426",
                    "value": "000000000"
                },
                {
                    "id": "ILN0436",
                    "value": "000000000"
                },
                {
                    "id": "ILN0437",
                    "value": "000000000"
                },
                {
                    "id": "ILN0438",
                    "value": "000000000"
                },
                {
                    "id": "ILN1300",
                    "value": "000000006"
                },
                {
                    "id": "ILN1360",
                    "value": "000000097"
                },
                {
                    "id": "ILN1370",
                    "value": "000000002"
                },
                {
                    "id": "ILN1380",
                    "value": "000000002"
                },
                {
                    "id": "ILN1401",
                    "value": "000000000"
                },
                {
                    "id": "ILN2000",
                    "value": "000000097"
                },
                {
                    "id": "ILN2106",
                    "value": "000000097"
                },
                {
                    "id": "ILN2126",
                    "value": "000000097"
                },
                {
                    "id": "ILN2136",
                    "value": "000000097"
                },
                {
                    "id": "ILN2176",
                    "value": "000000097"
                },
                {
                    "id": "ILN2206",
                    "value": "000000097"
                },
                {
                    "id": "ILN2320",
                    "value": "000000000"
                },
                {
                    "id": "ILN2326",
                    "value": "000000097"
                },
                {
                    "id": "ILN2327",
                    "value": "000000000"
                },
                {
                    "id": "ILN2328",
                    "value": "000000000"
                },
                {
                    "id": "ILN2350",
                    "value": "000000000"
                },
                {
                    "id": "ILN2356",
                    "value": "000000097"
                },
                {
                    "id": "ILN2357",
                    "value": "000000000"
                },
                {
                    "id": "ILN2358",
                    "value": "000000000"
                },
                {
                    "id": "ILN2380",
                    "value": "000000000"
                },
                {
                    "id": "ILN2386",
                    "value": "000000097"
                },
                {
                    "id": "ILN2387",
                    "value": "000000000"
                },
                {
                    "id": "ILN2388",
                    "value": "000000000"
                },
                {
                    "id": "ILN2800",
                    "value": "000000000"
                },
                {
                    "id": "ILN3110",
                    "value": "000000000"
                },
                {
                    "id": "ILN3484",
                    "value": "000000097"
                },
                {
                    "id": "ILN4070",
                    "value": "000000000"
                },
                {
                    "id": "ILN4080",
                    "value": "000000000"
                },
                {
                    "id": "ILN4370",
                    "value": "000000000"
                },
                {
                    "id": "ILN4380",
                    "value": "000000000"
                },
                {
                    "id": "ILN4770",
                    "value": "000000000"
                },
                {
                    "id": "ILN4780",
                    "value": "000000000"
                },
                {
                    "id": "ILN5020",
                    "value": "999999997"
                },
                {
                    "id": "ILN5040",
                    "value": "999999997"
                },
                {
                    "id": "ILN5043",
                    "value": "999999997"
                },
                {
                    "id": "ILN5047",
                    "value": "999999997"
                },
                {
                    "id": "ILN5070",
                    "value": "999999997"
                },
                {
                    "id": "ILN5120",
                    "value": "999999997"
                },
                {
                    "id": "ILN5122",
                    "value": "999999996"
                },
                {
                    "id": "ILN5220",
                    "value": "999999997"
                },
                {
                    "id": "ILN5238",
                    "value": "999999997"
                },
                {
                    "id": "ILN5320",
                    "value": "999999997"
                },
                {
                    "id": "ILN5400",
                    "value": "000020000"
                },
                {
                    "id": "ILN5420",
                    "value": "999999997"
                },
                {
                    "id": "ILN5422",
                    "value": "999999996"
                },
                {
                    "id": "ILN5520",
                    "value": "999999997"
                },
                {
                    "id": "ILN5740",
                    "value": "999999997"
                },
                {
                    "id": "ILN5743",
                    "value": "999999997"
                },
                {
                    "id": "ILN5747",
                    "value": "999999997"
                },
                {
                    "id": "ILN5770",
                    "value": "999999997"
                },
                {
                    "id": "ILN5820",
                    "value": "999999997"
                },
                {
                    "id": "ILN5823",
                    "value": "999999997"
                },
                {
                    "id": "ILN5824",
                    "value": "999999997"
                },
                {
                    "id": "ILN5838",
                    "value": "999999997"
                },
                {
                    "id": "ILN5923",
                    "value": "999999997"
                },
                {
                    "id": "ILN5926",
                    "value": "999999997"
                },
                {
                    "id": "ILN5930",
                    "value": "999999997"
                },
                {
                    "id": "ILN6160",
                    "value": "000000997"
                },
                {
                    "id": "ILN6200",
                    "value": "000000001"
                },
                {
                    "id": "ILN6210",
                    "value": "000000994"
                },
                {
                    "id": "ILN6220",
                    "value": "000000001"
                },
                {
                    "id": "ILN6230",
                    "value": "000000001"
                },
                {
                    "id": "ILN6270",
                    "value": "000000001"
                },
                {
                    "id": "ILN6280",
                    "value": "000000001"
                },
                {
                    "id": "ILN7110",
                    "value": "000000997"
                },
                {
                    "id": "ILN7120",
                    "value": "000000997"
                },
                {
                    "id": "ILN7130",
                    "value": "000000997"
                },
                {
                    "id": "ILN7150",
                    "value": "000000997"
                },
                {
                    "id": "ILN7300",
                    "value": "000000067"
                },
                {
                    "id": "ILN7310",
                    "value": "000000000"
                },
                {
                    "id": "ILN7313",
                    "value": "000000000"
                },
                {
                    "id": "ILN7410",
                    "value": "000000000"
                },
                {
                    "id": "ILN7430",
                    "value": "000000100"
                },
                {
                    "id": "ILN7432",
                    "value": "000000997"
                },
                {
                    "id": "ILN7433",
                    "value": "000000100"
                },
                {
                    "id": "ILN7434",
                    "value": "000000100"
                },
                {
                    "id": "ILN7436",
                    "value": "000000997"
                },
                {
                    "id": "ILN7437",
                    "value": "000000033"
                },
                {
                    "id": "ILN7438",
                    "value": "000000033"
                },
                {
                    "id": "ILN7440",
                    "value": "000000000"
                },
                {
                    "id": "ILN7450",
                    "value": "000000000"
                },
                {
                    "id": "ILN7460",
                    "value": "000000000"
                },
                {
                    "id": "ILN7470",
                    "value": "000000000"
                },
                {
                    "id": "ILN8120",
                    "value": "000000014"
                },
                {
                    "id": "ILN8140",
                    "value": "000009997"
                },
                {
                    "id": "ILN8150",
                    "value": "000009996"
                },
                {
                    "id": "ILN8151",
                    "value": "000009996"
                },
                {
                    "id": "ILN8152",
                    "value": "000009996"
                },
                {
                    "id": "ILN8153",
                    "value": "000009996"
                },
                {
                    "id": "ILN8155",
                    "value": "000009996"
                },
                {
                    "id": "ILN8157",
                    "value": "000009996"
                },
                {
                    "id": "ILN8160",
                    "value": "000009996"
                },
                {
                    "id": "ILN8220",
                    "value": "000000081"
                },
                {
                    "id": "ILN8222",
                    "value": "000009996"
                },
                {
                    "id": "ILN8320",
                    "value": "000000042"
                },
                {
                    "id": "IQA9410",
                    "value": "000000000"
                },
                {
                    "id": "IQA9415",
                    "value": "000000000"
                },
                {
                    "id": "IQA9416",
                    "value": "000000000"
                },
                {
                    "id": "IQA9417",
                    "value": "000000000"
                },
                {
                    "id": "IQA9426",
                    "value": "000000000"
                },
                {
                    "id": "IQA9427",
                    "value": "000000000"
                },
                {
                    "id": "IQA9510",
                    "value": "000009999"
                },
                {
                    "id": "IQA9540",
                    "value": "000009999"
                },
                {
                    "id": "IQB9410",
                    "value": "000000000"
                },
                {
                    "id": "IQB9415",
                    "value": "000000000"
                },
                {
                    "id": "IQB9416",
                    "value": "000000000"
                },
                {
                    "id": "IQB9417",
                    "value": "000000000"
                },
                {
                    "id": "IQB9510",
                    "value": "000009999"
                },
                {
                    "id": "IQB9540",
                    "value": "000009999"
                },
                {
                    "id": "IQC9410",
                    "value": "000000000"
                },
                {
                    "id": "IQC9415",
                    "value": "000000000"
                },
                {
                    "id": "IQC9416",
                    "value": "000000000"
                },
                {
                    "id": "IQC9417",
                    "value": "000000000"
                },
                {
                    "id": "IQF9410",
                    "value": "000000000"
                },
                {
                    "id": "IQF9415",
                    "value": "000000000"
                },
                {
                    "id": "IQF9416",
                    "value": "000000000"
                },
                {
                    "id": "IQF9417",
                    "value": "000000000"
                },
                {
                    "id": "IQF9510",
                    "value": "000009999"
                },
                {
                    "id": "IQF9540",
                    "value": "000009999"
                },
                {
                    "id": "IQM9410",
                    "value": "000000000"
                },
                {
                    "id": "IQM9415",
                    "value": "000000000"
                },
                {
                    "id": "IQM9416",
                    "value": "000000000"
                },
                {
                    "id": "IQM9417",
                    "value": "000000000"
                },
                {
                    "id": "IQM9510",
                    "value": "000009999"
                },
                {
                    "id": "IQM9540",
                    "value": "000009999"
                },
                {
                    "id": "IQP9510",
                    "value": "000009999"
                },
                {
                    "id": "IQP9540",
                    "value": "000009999"
                },
                {
                    "id": "IQR9410",
                    "value": "000000000"
                },
                {
                    "id": "IQR9415",
                    "value": "000000000"
                },
                {
                    "id": "IQR9416",
                    "value": "000000000"
                },
                {
                    "id": "IQR9417",
                    "value": "000000000"
                },
                {
                    "id": "IQR9510",
                    "value": "000009999"
                },
                {
                    "id": "IQR9540",
                    "value": "000009999"
                },
                {
                    "id": "IQT9410",
                    "value": "000000000"
                },
                {
                    "id": "IQT9412",
                    "value": "000000000"
                },
                {
                    "id": "IQT9413",
                    "value": "000000000"
                },
                {
                    "id": "IQT9415",
                    "value": "000000000"
                },
                {
                    "id": "IQT9416",
                    "value": "000000000"
                },
                {
                    "id": "IQT9417",
                    "value": "000000000"
                },
                {
                    "id": "IQT9420",
                    "value": "000000000"
                },
                {
                    "id": "IQT9421",
                    "value": "000000000"
                },
                {
                    "id": "IQT9422",
                    "value": "000000000"
                },
                {
                    "id": "IQT9423",
                    "value": "000000000"
                },
                {
                    "id": "IQT9425",
                    "value": "000000000"
                },
                {
                    "id": "IQT9426",
                    "value": "000000000"
                },
                {
                    "id": "IQT9427",
                    "value": "000000000"
                },
                {
                    "id": "IQT9510",
                    "value": "000009999"
                },
                {
                    "id": "IQT9523",
                    "value": "000000000"
                },
                {
                    "id": "IQT9525",
                    "value": "000000000"
                },
                {
                    "id": "IQT9526",
                    "value": "000000000"
                },
                {
                    "id": "IQT9533",
                    "value": "000000994"
                },
                {
                    "id": "IQT9535",
                    "value": "000000994"
                },
                {
                    "id": "IQT9536",
                    "value": "000000994"
                },
                {
                    "id": "IQT9846",
                    "value": "000000994"
                },
                {
                    "id": "MFX5029",
                    "value": "999999998"
                },
                {
                    "id": "MFX5829",
                    "value": "999999998"
                },
                {
                    "id": "MTA0300",
                    "value": "000000000"
                },
                {
                    "id": "MTA0304",
                    "value": "000000000"
                },
                {
                    "id": "MTA0316",
                    "value": "000000098"
                },
                {
                    "id": "MTA0317",
                    "value": "000000098"
                },
                {
                    "id": "MTA0318",
                    "value": "000000098"
                },
                {
                    "id": "MTA0400",
                    "value": "000000098"
                },
                {
                    "id": "MTA0416",
                    "value": "000000098"
                },
                {
                    "id": "MTA0436",
                    "value": "000000098"
                },
                {
                    "id": "MTA0437",
                    "value": "000000098"
                },
                {
                    "id": "MTA0438",
                    "value": "000000098"
                },
                {
                    "id": "MTA0700",
                    "value": "000000098"
                },
                {
                    "id": "MTA0702",
                    "value": "000000098"
                },
                {
                    "id": "MTA1300",
                    "value": "000000098"
                },
                {
                    "id": "MTA1304",
                    "value": "000000098"
                },
                {
                    "id": "MTA1360",
                    "value": "000000098"
                },
                {
                    "id": "MTA1370",
                    "value": "000000098"
                },
                {
                    "id": "MTA1380",
                    "value": "000000098"
                },
                {
                    "id": "MTA2000",
                    "value": "000000098"
                },
                {
                    "id": "MTA2106",
                    "value": "000000098"
                },
                {
                    "id": "MTA2126",
                    "value": "000000098"
                },
                {
                    "id": "MTA2136",
                    "value": "000000098"
                },
                {
                    "id": "MTA2176",
                    "value": "000000098"
                },
                {
                    "id": "MTA2206",
                    "value": "000000098"
                },
                {
                    "id": "MTA2320",
                    "value": "000000098"
                },
                {
                    "id": "MTA2326",
                    "value": "000000098"
                },
                {
                    "id": "MTA2327",
                    "value": "000000098"
                },
                {
                    "id": "MTA2328",
                    "value": "000000098"
                },
                {
                    "id": "MTA2350",
                    "value": "000000098"
                },
                {
                    "id": "MTA2356",
                    "value": "000000098"
                },
                {
                    "id": "MTA2357",
                    "value": "000000098"
                },
                {
                    "id": "MTA2358",
                    "value": "000000098"
                },
                {
                    "id": "MTA2380",
                    "value": "000000098"
                },
                {
                    "id": "MTA2386",
                    "value": "000000098"
                },
                {
                    "id": "MTA2387",
                    "value": "000000098"
                },
                {
                    "id": "MTA2388",
                    "value": "000000098"
                },
                {
                    "id": "MTA2800",
                    "value": "000000098"
                },
                {
                    "id": "MTA4070",
                    "value": "000000998"
                },
                {
                    "id": "MTA4080",
                    "value": "000000998"
                },
                {
                    "id": "MTA4370",
                    "value": "000000998"
                },
                {
                    "id": "MTA4380",
                    "value": "000000998"
                },
                {
                    "id": "MTA4770",
                    "value": "000000998"
                },
                {
                    "id": "MTA4780",
                    "value": "000000998"
                },
                {
                    "id": "MTA5020",
                    "value": "999999998"
                },
                {
                    "id": "MTA5030",
                    "value": "999999998"
                },
                {
                    "id": "MTA5040",
                    "value": "999999998"
                },
                {
                    "id": "MTA5043",
                    "value": "999999998"
                },
                {
                    "id": "MTA5047",
                    "value": "999999998"
                },
                {
                    "id": "MTA5070",
                    "value": "999999998"
                },
                {
                    "id": "MTA5121",
                    "value": "999999998"
                },
                {
                    "id": "MTA5320",
                    "value": "999999998"
                },
                {
                    "id": "MTA5400",
                    "value": "999999998"
                },
                {
                    "id": "MTA5420",
                    "value": "999999998"
                },
                {
                    "id": "MTA5421",
                    "value": "999999998"
                },
                {
                    "id": "MTA5422",
                    "value": "999999998"
                },
                {
                    "id": "MTA5740",
                    "value": "999999998"
                },
                {
                    "id": "MTA5742",
                    "value": "999999998"
                },
                {
                    "id": "MTA5743",
                    "value": "999999998"
                },
                {
                    "id": "MTA5747",
                    "value": "999999998"
                },
                {
                    "id": "MTA5770",
                    "value": "999999998"
                },
                {
                    "id": "MTA5830",
                    "value": "999999998"
                },
                {
                    "id": "MTA6160",
                    "value": "000000998"
                },
                {
                    "id": "MTA6200",
                    "value": "000000998"
                },
                {
                    "id": "MTA6210",
                    "value": "000000998"
                },
                {
                    "id": "MTA6220",
                    "value": "000000998"
                },
                {
                    "id": "MTA6230",
                    "value": "000000998"
                },
                {
                    "id": "MTA6270",
                    "value": "000000998"
                },
                {
                    "id": "MTA6280",
                    "value": "000000998"
                },
                {
                    "id": "MTA7410",
                    "value": "000000998"
                },
                {
                    "id": "MTA7430",
                    "value": "000000998"
                },
                {
                    "id": "MTA7432",
                    "value": "000000998"
                },
                {
                    "id": "MTA7433",
                    "value": "000000998"
                },
                {
                    "id": "MTA7434",
                    "value": "000000998"
                },
                {
                    "id": "MTA7436",
                    "value": "000000998"
                },
                {
                    "id": "MTA7437",
                    "value": "000000998"
                },
                {
                    "id": "MTA7438",
                    "value": "000000998"
                },
                {
                    "id": "MTA7440",
                    "value": "000000998"
                },
                {
                    "id": "MTA7450",
                    "value": "000000998"
                },
                {
                    "id": "MTA7460",
                    "value": "000000998"
                },
                {
                    "id": "MTA7470",
                    "value": "000000998"
                },
                {
                    "id": "MTA8120",
                    "value": "000009998"
                },
                {
                    "id": "MTA8150",
                    "value": "000009998"
                },
                {
                    "id": "MTA8151",
                    "value": "000009998"
                },
                {
                    "id": "MTA8153",
                    "value": "000009998"
                },
                {
                    "id": "MTA8157",
                    "value": "000009998"
                },
                {
                    "id": "MTA8160",
                    "value": "000009998"
                },
                {
                    "id": "MTA8220",
                    "value": "000009998"
                },
                {
                    "id": "MTA8320",
                    "value": "000009998"
                },
                {
                    "id": "MTA8370",
                    "value": "000009998"
                },
                {
                    "id": "MTF0153",
                    "value": "000000098"
                },
                {
                    "id": "MTF0154",
                    "value": "000000098"
                },
                {
                    "id": "MTF0155",
                    "value": "000000098"
                },
                {
                    "id": "MTF0156",
                    "value": "000000098"
                },
                {
                    "id": "MTF0157",
                    "value": "000000098"
                },
                {
                    "id": "MTF0300",
                    "value": "000000000"
                },
                {
                    "id": "MTF0416",
                    "value": "000000098"
                },
                {
                    "id": "MTF0436",
                    "value": "000000098"
                },
                {
                    "id": "MTF0471",
                    "value": "000000098"
                },
                {
                    "id": "MTF0476",
                    "value": "000000098"
                },
                {
                    "id": "MTF0481",
                    "value": "000000098"
                },
                {
                    "id": "MTF0486",
                    "value": "000000098"
                },
                {
                    "id": "MTF0700",
                    "value": "000000098"
                },
                {
                    "id": "MTF1300",
                    "value": "000000098"
                },
                {
                    "id": "MTF1471",
                    "value": "000000098"
                },
                {
                    "id": "MTF1472",
                    "value": "000000098"
                },
                {
                    "id": "MTF2000",
                    "value": "000000098"
                },
                {
                    "id": "MTF2146",
                    "value": "000000098"
                },
                {
                    "id": "MTF2156",
                    "value": "000000098"
                },
                {
                    "id": "MTF2176",
                    "value": "000000098"
                },
                {
                    "id": "MTF2196",
                    "value": "000000098"
                },
                {
                    "id": "MTF2320",
                    "value": "000000098"
                },
                {
                    "id": "MTF2350",
                    "value": "000000098"
                },
                {
                    "id": "MTF2358",
                    "value": "000000098"
                },
                {
                    "id": "MTF2380",
                    "value": "000000098"
                },
                {
                    "id": "MTF2388",
                    "value": "000000098"
                },
                {
                    "id": "MTF2860",
                    "value": "000000098"
                },
                {
                    "id": "MTF2861",
                    "value": "000000098"
                },
                {
                    "id": "MTF2862",
                    "value": "000000098"
                },
                {
                    "id": "MTF2863",
                    "value": "000000098"
                },
                {
                    "id": "MTF2864",
                    "value": "000000098"
                },
                {
                    "id": "MTF2867",
                    "value": "000000098"
                },
                {
                    "id": "MTF2868",
                    "value": "000000098"
                },
                {
                    "id": "MTF2930",
                    "value": "000000098"
                },
                {
                    "id": "MTF2933",
                    "value": "000000098"
                },
                {
                    "id": "MTF2934",
                    "value": "000000098"
                },
                {
                    "id": "MTF2935",
                    "value": "000000098"
                },
                {
                    "id": "MTF2936",
                    "value": "000000098"
                },
                {
                    "id": "MTF2960",
                    "value": "000000098"
                },
                {
                    "id": "MTF2963",
                    "value": "000000098"
                },
                {
                    "id": "MTF2974",
                    "value": "000000098"
                },
                {
                    "id": "MTF2975",
                    "value": "000000098"
                },
                {
                    "id": "MTF2976",
                    "value": "000000098"
                },
                {
                    "id": "MTF3485",
                    "value": "000000098"
                },
                {
                    "id": "MTF4170",
                    "value": "000000998"
                },
                {
                    "id": "MTF4180",
                    "value": "000000998"
                },
                {
                    "id": "MTF4250",
                    "value": "000000998"
                },
                {
                    "id": "MTF4260",
                    "value": "000000998"
                },
                {
                    "id": "MTF4270",
                    "value": "000000998"
                },
                {
                    "id": "MTF4470",
                    "value": "000000998"
                },
                {
                    "id": "MTF4480",
                    "value": "000000998"
                },
                {
                    "id": "MTF4550",
                    "value": "000000998"
                },
                {
                    "id": "MTF4560",
                    "value": "000000998"
                },
                {
                    "id": "MTF4570",
                    "value": "000000998"
                },
                {
                    "id": "MTF4770",
                    "value": "000000998"
                },
                {
                    "id": "MTF4780",
                    "value": "000000998"
                },
                {
                    "id": "MTF4850",
                    "value": "000000998"
                },
                {
                    "id": "MTF4860",
                    "value": "000000998"
                },
                {
                    "id": "MTF4870",
                    "value": "000000998"
                },
                {
                    "id": "MTF5020",
                    "value": "999999998"
                },
                {
                    "id": "MTF5022",
                    "value": "999999998"
                },
                {
                    "id": "MTF5023",
                    "value": "999999998"
                },
                {
                    "id": "MTF5080",
                    "value": "999999998"
                },
                {
                    "id": "MTF5081",
                    "value": "999999998"
                },
                {
                    "id": "MTF5082",
                    "value": "999999998"
                },
                {
                    "id": "MTF5083",
                    "value": "999999998"
                },
                {
                    "id": "MTF5100",
                    "value": "999999998"
                },
                {
                    "id": "MTF5121",
                    "value": "999999998"
                },
                {
                    "id": "MTF5122",
                    "value": "999999998"
                },
                {
                    "id": "MTF5124",
                    "value": "999999998"
                },
                {
                    "id": "MTF5125",
                    "value": "999999998"
                },
                {
                    "id": "MTF5128",
                    "value": "999999998"
                },
                {
                    "id": "MTF5129",
                    "value": "999999998"
                },
                {
                    "id": "MTF5220",
                    "value": "999999998"
                },
                {
                    "id": "MTF5238",
                    "value": "999999998"
                },
                {
                    "id": "MTF5320",
                    "value": "999999998"
                },
                {
                    "id": "MTF5322",
                    "value": "999999998"
                },
                {
                    "id": "MTF5323",
                    "value": "999999998"
                },
                {
                    "id": "MTF5351",
                    "value": "999999998"
                },
                {
                    "id": "MTF5352",
                    "value": "999999998"
                },
                {
                    "id": "MTF5353",
                    "value": "999999998"
                },
                {
                    "id": "MTF5354",
                    "value": "999999998"
                },
                {
                    "id": "MTF5355",
                    "value": "999999998"
                },
                {
                    "id": "MTF5356",
                    "value": "999999998"
                },
                {
                    "id": "MTF5357",
                    "value": "999999998"
                },
                {
                    "id": "MTF5358",
                    "value": "999999998"
                },
                {
                    "id": "MTF5420",
                    "value": "999999998"
                },
                {
                    "id": "MTF5421",
                    "value": "999999998"
                },
                {
                    "id": "MTF5422",
                    "value": "999999998"
                },
                {
                    "id": "MTF5428",
                    "value": "999999998"
                },
                {
                    "id": "MTF5429",
                    "value": "999999998"
                },
                {
                    "id": "MTF5620",
                    "value": "999999998"
                },
                {
                    "id": "MTF5820",
                    "value": "999999998"
                },
                {
                    "id": "MTF5838",
                    "value": "999999998"
                },
                {
                    "id": "MTF5930",
                    "value": "999999998"
                },
                {
                    "id": "MTF6200",
                    "value": "000000998"
                },
                {
                    "id": "MTF6243",
                    "value": "000000998"
                },
                {
                    "id": "MTF6244",
                    "value": "000000998"
                },
                {
                    "id": "MTF6245",
                    "value": "000000998"
                },
                {
                    "id": "MTF6246",
                    "value": "000000998"
                },
                {
                    "id": "MTF6247",
                    "value": "000000998"
                },
                {
                    "id": "MTF6280",
                    "value": "000000998"
                },
                {
                    "id": "MTF6281",
                    "value": "000000998"
                },
                {
                    "id": "MTF6282",
                    "value": "000000998"
                },
                {
                    "id": "MTF6326",
                    "value": "000000098"
                },
                {
                    "id": "MTF7110",
                    "value": "000000998"
                },
                {
                    "id": "MTF7150",
                    "value": "000000998"
                },
                {
                    "id": "MTF8111",
                    "value": "000009998"
                },
                {
                    "id": "MTF8120",
                    "value": "000009998"
                },
                {
                    "id": "MTF8122",
                    "value": "000009998"
                },
                {
                    "id": "MTF8128",
                    "value": "000009998"
                },
                {
                    "id": "MTF8129",
                    "value": "000009998"
                },
                {
                    "id": "MTF8132",
                    "value": "000099998"
                },
                {
                    "id": "MTF8140",
                    "value": "000009998"
                },
                {
                    "id": "MTF8141",
                    "value": "000009998"
                },
                {
                    "id": "MTF8151",
                    "value": "000009998"
                },
                {
                    "id": "MTF8166",
                    "value": "000009998"
                },
                {
                    "id": "MTF8169",
                    "value": "000009998"
                },
                {
                    "id": "MTF8220",
                    "value": "000009998"
                },
                {
                    "id": "MTF8222",
                    "value": "000009998"
                },
                {
                    "id": "MTF8320",
                    "value": "000009998"
                },
                {
                    "id": "MTF8810",
                    "value": "000009998"
                },
                {
                    "id": "MTJ0300",
                    "value": "000000000"
                },
                {
                    "id": "MTJ0316",
                    "value": "000000098"
                },
                {
                    "id": "MTJ0416",
                    "value": "000000098"
                },
                {
                    "id": "MTJ5030",
                    "value": "999999998"
                },
                {
                    "id": "MTJ5320",
                    "value": "999999998"
                },
                {
                    "id": "MTJ5730",
                    "value": "999999998"
                },
                {
                    "id": "MTJ5820",
                    "value": "999999998"
                },
                {
                    "id": "MTJ8120",
                    "value": "000009998"
                },
                {
                    "id": "MTJ8220",
                    "value": "000009998"
                },
                {
                    "id": "MTS0300",
                    "value": "000000000"
                },
                {
                    "id": "MTS0416",
                    "value": "000000098"
                },
                {
                    "id": "MTS0436",
                    "value": "000000098"
                },
                {
                    "id": "MTS0437",
                    "value": "000000098"
                },
                {
                    "id": "MTS0438",
                    "value": "000000098"
                },
                {
                    "id": "MTS0700",
                    "value": "000000098"
                },
                {
                    "id": "MTS1300",
                    "value": "000000098"
                },
                {
                    "id": "MTS2000",
                    "value": "000000098"
                },
                {
                    "id": "MTS2156",
                    "value": "000000098"
                },
                {
                    "id": "MTS2320",
                    "value": "000000098"
                },
                {
                    "id": "MTS2350",
                    "value": "000000098"
                },
                {
                    "id": "MTS2358",
                    "value": "000000098"
                },
                {
                    "id": "MTS2380",
                    "value": "000000098"
                },
                {
                    "id": "MTS2388",
                    "value": "000000098"
                },
                {
                    "id": "MTS4180",
                    "value": "000000998"
                },
                {
                    "id": "MTS5020",
                    "value": "999999998"
                },
                {
                    "id": "MTS5120",
                    "value": "999999998"
                },
                {
                    "id": "MTS5122",
                    "value": "999999998"
                },
                {
                    "id": "MTS5320",
                    "value": "999999998"
                },
                {
                    "id": "MTS5420",
                    "value": "999999998"
                },
                {
                    "id": "MTS5422",
                    "value": "999999998"
                },
                {
                    "id": "MTS5620",
                    "value": "999999998"
                },
                {
                    "id": "MTS5820",
                    "value": "999999998"
                },
                {
                    "id": "MTS6200",
                    "value": "000000998"
                },
                {
                    "id": "MTS6280",
                    "value": "000000998"
                },
                {
                    "id": "MTS7110",
                    "value": "000000998"
                },
                {
                    "id": "MTS7130",
                    "value": "000000998"
                },
                {
                    "id": "MTS8120",
                    "value": "000009998"
                },
                {
                    "id": "MTS8122",
                    "value": "000009998"
                },
                {
                    "id": "MTS8132",
                    "value": "000099998"
                },
                {
                    "id": "MTS8151",
                    "value": "000009998"
                },
                {
                    "id": "MTS8220",
                    "value": "000009998"
                },
                {
                    "id": "MTS8222",
                    "value": "000009998"
                },
                {
                    "id": "MTS8320",
                    "value": "000009998"
                },
                {
                    "id": "MTX5039",
                    "value": "999999998"
                },
                {
                    "id": "MTX5839",
                    "value": "999999998"
                },
                {
                    "id": "PIL0300",
                    "value": "000000006"
                },
                {
                    "id": "PIL0438",
                    "value": "000000000"
                },
                {
                    "id": "PIL1300",
                    "value": "000000006"
                },
                {
                    "id": "PIL2328",
                    "value": "000000000"
                },
                {
                    "id": "PIL2358",
                    "value": "000000000"
                },
                {
                    "id": "PIL2388",
                    "value": "000000000"
                },
                {
                    "id": "PIL2800",
                    "value": "000000000"
                },
                {
                    "id": "PIL5020",
                    "value": "999999997"
                },
                {
                    "id": "PIL5320",
                    "value": "999999997"
                },
                {
                    "id": "PIL6200",
                    "value": "000000001"
                },
                {
                    "id": "PIL8120",
                    "value": "000000014"
                },
                {
                    "id": "PIL8132",
                    "value": "000099996"
                },
                {
                    "id": "PIL8220",
                    "value": "000000081"
                },
                {
                    "id": "REC0416",
                    "value": "000000098"
                },
                {
                    "id": "REC5020",
                    "value": "999999998"
                },
                {
                    "id": "REC5120",
                    "value": "999999998"
                },
                {
                    "id": "REC7110",
                    "value": "000000998"
                },
                {
                    "id": "REC7130",
                    "value": "000000998"
                },
                {
                    "id": "REC8122",
                    "value": "000009998"
                },
                {
                    "id": "REC8132",
                    "value": "000099998"
                },
                {
                    "id": "REH0300",
                    "value": "000000003"
                },
                {
                    "id": "REH0416",
                    "value": "000000002"
                },
                {
                    "id": "REH0437",
                    "value": "000000000"
                },
                {
                    "id": "REH2000",
                    "value": "000000002"
                },
                {
                    "id": "REH2687",
                    "value": "000000000"
                },
                {
                    "id": "REH2688",
                    "value": "000000000"
                },
                {
                    "id": "REH3410",
                    "value": "000000001"
                },
                {
                    "id": "REH3422",
                    "value": "000000000"
                },
                {
                    "id": "REH3423",
                    "value": "000000000"
                },
                {
                    "id": "REH3510",
                    "value": "000000001"
                },
                {
                    "id": "REH5020",
                    "value": "000000248"
                },
                {
                    "id": "REH5030",
                    "value": "000000248"
                },
                {
                    "id": "REH5120",
                    "value": "000000248"
                },
                {
                    "id": "REH5320",
                    "value": "000021000"
                },
                {
                    "id": "REH5420",
                    "value": "000013500"
                },
                {
                    "id": "REH5820",
                    "value": "000000010"
                },
                {
                    "id": "REH5830",
                    "value": "000000010"
                },
                {
                    "id": "REH7110",
                    "value": "000000001"
                },
                {
                    "id": "REH7120",
                    "value": "000000002"
                },
                {
                    "id": "REH8120",
                    "value": "000000149"
                },
                {
                    "id": "REH8127",
                    "value": "000000149"
                },
                {
                    "id": "REH8220",
                    "value": "000000469"
                },
                {
                    "id": "REH8227",
                    "value": "000000469"
                },
                {
                    "id": "REJ0300",
                    "value": "000000000"
                },
                {
                    "id": "REJ0316",
                    "value": "000000098"
                },
                {
                    "id": "REJ0416",
                    "value": "000000098"
                },
                {
                    "id": "REJ5030",
                    "value": "999999998"
                },
                {
                    "id": "REJ5320",
                    "value": "999999998"
                },
                {
                    "id": "REJ5820",
                    "value": "999999998"
                },
                {
                    "id": "REJ5830",
                    "value": "999999998"
                },
                {
                    "id": "REJ8120",
                    "value": "000009998"
                },
                {
                    "id": "REJ8220",
                    "value": "000009998"
                },
                {
                    "id": "REV0300",
                    "value": "000000003"
                },
                {
                    "id": "REV0316",
                    "value": "000000002"
                },
                {
                    "id": "REV0317",
                    "value": "000000002"
                },
                {
                    "id": "REV0318",
                    "value": "000000002"
                },
                {
                    "id": "REV0416",
                    "value": "000000002"
                },
                {
                    "id": "REV0436",
                    "value": "000000000"
                },
                {
                    "id": "REV0437",
                    "value": "000000000"
                },
                {
                    "id": "REV0438",
                    "value": "000000000"
                },
                {
                    "id": "REV1300",
                    "value": "000000003"
                },
                {
                    "id": "REV1360",
                    "value": "000000002"
                },
                {
                    "id": "REV1370",
                    "value": "000000002"
                },
                {
                    "id": "REV1380",
                    "value": "000000002"
                },
                {
                    "id": "REV1401",
                    "value": "000000001"
                },
                {
                    "id": "REV2106",
                    "value": "000000000"
                },
                {
                    "id": "REV2126",
                    "value": "000000000"
                },
                {
                    "id": "REV2136",
                    "value": "000000000"
                },
                {
                    "id": "REV2176",
                    "value": "000000000"
                },
                {
                    "id": "REV2206",
                    "value": "000000000"
                },
                {
                    "id": "REV2320",
                    "value": "000000000"
                },
                {
                    "id": "REV2326",
                    "value": "000000000"
                },
                {
                    "id": "REV2327",
                    "value": "000000000"
                },
                {
                    "id": "REV2328",
                    "value": "000000000"
                },
                {
                    "id": "REV2350",
                    "value": "000000000"
                },
                {
                    "id": "REV2356",
                    "value": "000000000"
                },
                {
                    "id": "REV2357",
                    "value": "000000000"
                },
                {
                    "id": "REV2358",
                    "value": "000000000"
                },
                {
                    "id": "REV2380",
                    "value": "000000000"
                },
                {
                    "id": "REV2386",
                    "value": "000000000"
                },
                {
                    "id": "REV2387",
                    "value": "000000000"
                },
                {
                    "id": "REV2388",
                    "value": "000000000"
                },
                {
                    "id": "REV2800",
                    "value": "000000000"
                },
                {
                    "id": "REV2840",
                    "value": "000000000"
                },
                {
                    "id": "REV2841",
                    "value": "000000000"
                },
                {
                    "id": "REV3421",
                    "value": "000000000"
                },
                {
                    "id": "REV3422",
                    "value": "000000000"
                },
                {
                    "id": "REV3423",
                    "value": "000000000"
                },
                {
                    "id": "REV3424",
                    "value": "000000000"
                },
                {
                    "id": "REV3510",
                    "value": "000000001"
                },
                {
                    "id": "REV3511",
                    "value": "000000000"
                },
                {
                    "id": "REV3517",
                    "value": "000000000"
                },
                {
                    "id": "REV4070",
                    "value": "000000000"
                },
                {
                    "id": "REV4080",
                    "value": "000000000"
                },
                {
                    "id": "REV4170",
                    "value": "000000000"
                },
                {
                    "id": "REV4370",
                    "value": "000000000"
                },
                {
                    "id": "REV4380",
                    "value": "000000000"
                },
                {
                    "id": "REV4770",
                    "value": "000000000"
                },
                {
                    "id": "REV4780",
                    "value": "000000000"
                },
                {
                    "id": "REV5002",
                    "value": "000000248"
                },
                {
                    "id": "REV5020",
                    "value": "000000248"
                },
                {
                    "id": "REV5030",
                    "value": "000000248"
                },
                {
                    "id": "REV5032",
                    "value": "000000248"
                },
                {
                    "id": "REV5036",
                    "value": "999999994"
                },
                {
                    "id": "REV5040",
                    "value": "000000000"
                },
                {
                    "id": "REV5043",
                    "value": "000000000"
                },
                {
                    "id": "REV5047",
                    "value": "000000000"
                },
                {
                    "id": "REV5070",
                    "value": "000000000"
                },
                {
                    "id": "REV5320",
                    "value": "000021000"
                },
                {
                    "id": "REV5324",
                    "value": "000021000"
                },
                {
                    "id": "REV5420",
                    "value": "000013500"
                },
                {
                    "id": "REV5620",
                    "value": "000020752"
                },
                {
                    "id": "REV5627",
                    "value": "000020752"
                },
                {
                    "id": "REV5740",
                    "value": "000000000"
                },
                {
                    "id": "REV5742",
                    "value": "000000000"
                },
                {
                    "id": "REV5743",
                    "value": "000000000"
                },
                {
                    "id": "REV5747",
                    "value": "000000000"
                },
                {
                    "id": "REV5770",
                    "value": "000000000"
                },
                {
                    "id": "REV5830",
                    "value": "000000010"
                },
                {
                    "id": "REV6160",
                    "value": "000000001"
                },
                {
                    "id": "REV6200",
                    "value": "000000001"
                },
                {
                    "id": "REV6210",
                    "value": "000000994"
                },
                {
                    "id": "REV6220",
                    "value": "000000994"
                },
                {
                    "id": "REV6230",
                    "value": "000000994"
                },
                {
                    "id": "REV6270",
                    "value": "000000001"
                },
                {
                    "id": "REV6280",
                    "value": "000000001"
                },
                {
                    "id": "REV7110",
                    "value": "000000001"
                },
                {
                    "id": "REV7112",
                    "value": "000000001"
                },
                {
                    "id": "REV7114",
                    "value": "000000001"
                },
                {
                    "id": "REV7140",
                    "value": "000000001"
                },
                {
                    "id": "REV7410",
                    "value": "000000067"
                },
                {
                    "id": "REV7420",
                    "value": "000000033"
                },
                {
                    "id": "REV7430",
                    "value": "000000100"
                },
                {
                    "id": "REV7432",
                    "value": "000000100"
                },
                {
                    "id": "REV7433",
                    "value": "000000100"
                },
                {
                    "id": "REV7434",
                    "value": "000000100"
                },
                {
                    "id": "REV7436",
                    "value": "000000067"
                },
                {
                    "id": "REV7437",
                    "value": "000000067"
                },
                {
                    "id": "REV7438",
                    "value": "000000067"
                },
                {
                    "id": "REV7440",
                    "value": "000000000"
                },
                {
                    "id": "REV7442",
                    "value": "000000000"
                },
                {
                    "id": "REV7443",
                    "value": "000000000"
                },
                {
                    "id": "REV7446",
                    "value": "000000000"
                },
                {
                    "id": "REV7447",
                    "value": "000000000"
                },
                {
                    "id": "REV7450",
                    "value": "000000000"
                },
                {
                    "id": "REV7460",
                    "value": "000000000"
                },
                {
                    "id": "REV7470",
                    "value": "000000000"
                },
                {
                    "id": "REV7800",
                    "value": "000002075"
                },
                {
                    "id": "REV7801",
                    "value": "999999994"
                },
                {
                    "id": "REV8120",
                    "value": "000000149"
                },
                {
                    "id": "REV8150",
                    "value": "000009996"
                },
                {
                    "id": "REV8151",
                    "value": "000009996"
                },
                {
                    "id": "REV8153",
                    "value": "000009996"
                },
                {
                    "id": "REV8157",
                    "value": "000009996"
                },
                {
                    "id": "REV8160",
                    "value": "000009996"
                },
                {
                    "id": "REV8220",
                    "value": "000000469"
                },
                {
                    "id": "REV8320",
                    "value": "000000298"
                },
                {
                    "id": "RPM0300",
                    "value": "000000000"
                },
                {
                    "id": "RPM2320",
                    "value": "000000098"
                },
                {
                    "id": "RPM5020",
                    "value": "999999998"
                },
                {
                    "id": "RPM5320",
                    "value": "999999998"
                },
                {
                    "id": "RPM5820",
                    "value": "999999998"
                },
                {
                    "id": "RPM6160",
                    "value": "000000998"
                },
                {
                    "id": "RPM6200",
                    "value": "000000998"
                },
                {
                    "id": "RPM7110",
                    "value": "000000998"
                },
                {
                    "id": "RPM8100",
                    "value": "000009998"
                },
                {
                    "id": "RPM8220",
                    "value": "000009998"
                },
                {
                    "id": "RTA0300",
                    "value": "000000001"
                },
                {
                    "id": "RTA0416",
                    "value": "000000001"
                },
                {
                    "id": "RTA0436",
                    "value": "000000000"
                },
                {
                    "id": "RTA1300",
                    "value": "000000001"
                },
                {
                    "id": "RTA2306",
                    "value": "000000000"
                },
                {
                    "id": "RTA2307",
                    "value": "000000000"
                },
                {
                    "id": "RTA2308",
                    "value": "000000000"
                },
                {
                    "id": "RTA2336",
                    "value": "000000000"
                },
                {
                    "id": "RTA2337",
                    "value": "000000000"
                },
                {
                    "id": "RTA2338",
                    "value": "000000000"
                },
                {
                    "id": "RTA2366",
                    "value": "000000000"
                },
                {
                    "id": "RTA2367",
                    "value": "000000000"
                },
                {
                    "id": "RTA2368",
                    "value": "000000000"
                },
                {
                    "id": "RTA2388",
                    "value": "000000000"
                },
                {
                    "id": "RTA3110",
                    "value": "000000000"
                },
                {
                    "id": "RTA4180",
                    "value": "000000000"
                },
                {
                    "id": "RTA5030",
                    "value": "000000000"
                },
                {
                    "id": "RTA5120",
                    "value": "000000000"
                },
                {
                    "id": "RTA5320",
                    "value": "000007500"
                },
                {
                    "id": "RTA5830",
                    "value": "000000000"
                },
                {
                    "id": "RTA6200",
                    "value": "000000001"
                },
                {
                    "id": "RTA6280",
                    "value": "000000001"
                },
                {
                    "id": "RTA7110",
                    "value": "000000000"
                },
                {
                    "id": "RTA7300",
                    "value": "000000011"
                },
                {
                    "id": "RTA8120",
                    "value": "000000149"
                },
                {
                    "id": "RTA8122",
                    "value": "000000149"
                },
                {
                    "id": "RTA8132",
                    "value": "000004550"
                },
                {
                    "id": "RTA8151",
                    "value": "000009996"
                },
                {
                    "id": "RTA8320",
                    "value": "000000149"
                },
                {
                    "id": "RTI0300",
                    "value": "000000000"
                },
                {
                    "id": "RTI0436",
                    "value": "000000098"
                },
                {
                    "id": "RTI4180",
                    "value": "000000998"
                },
                {
                    "id": "RTI5020",
                    "value": "999999998"
                },
                {
                    "id": "RTI5320",
                    "value": "999999998"
                },
                {
                    "id": "RTI5820",
                    "value": "999999998"
                },
                {
                    "id": "RTR0300",
                    "value": "000000001"
                },
                {
                    "id": "RTR0416",
                    "value": "000000001"
                },
                {
                    "id": "RTR0436",
                    "value": "000000000"
                },
                {
                    "id": "RTR0437",
                    "value": "000000000"
                },
                {
                    "id": "RTR0438",
                    "value": "000000000"
                },
                {
                    "id": "RTR1300",
                    "value": "000000001"
                },
                {
                    "id": "RTR1380",
                    "value": "000000001"
                },
                {
                    "id": "RTR2320",
                    "value": "000000000"
                },
                {
                    "id": "RTR2328",
                    "value": "000000000"
                },
                {
                    "id": "RTR2358",
                    "value": "000000000"
                },
                {
                    "id": "RTR2380",
                    "value": "000000000"
                },
                {
                    "id": "RTR2388",
                    "value": "000000000"
                },
                {
                    "id": "RTR2800",
                    "value": "000000000"
                },
                {
                    "id": "RTR3347",
                    "value": "000000094"
                },
                {
                    "id": "RTR3348",
                    "value": "000000000"
                },
                {
                    "id": "RTR3422",
                    "value": "000000000"
                },
                {
                    "id": "RTR3424",
                    "value": "000000000"
                },
                {
                    "id": "RTR3510",
                    "value": "000000000"
                },
                {
                    "id": "RTR3511",
                    "value": "000000000"
                },
                {
                    "id": "RTR5020",
                    "value": "000000000"
                },
                {
                    "id": "RTR5030",
                    "value": "000000000"
                },
                {
                    "id": "RTR5038",
                    "value": "999999996"
                },
                {
                    "id": "RTR5227",
                    "value": "999999995"
                },
                {
                    "id": "RTR5228",
                    "value": "999999995"
                },
                {
                    "id": "RTR5320",
                    "value": "000007500"
                },
                {
                    "id": "RTR5420",
                    "value": "000007500"
                },
                {
                    "id": "RTR5520",
                    "value": "000007500"
                },
                {
                    "id": "RTR5620",
                    "value": "000007500"
                },
                {
                    "id": "RTR5627",
                    "value": "000007500"
                },
                {
                    "id": "RTR5830",
                    "value": "000000000"
                },
                {
                    "id": "RTR5920",
                    "value": "000000000"
                },
                {
                    "id": "RTR5930",
                    "value": "000000000"
                },
                {
                    "id": "RTR6200",
                    "value": "000000001"
                },
                {
                    "id": "RTR6280",
                    "value": "000000001"
                },
                {
                    "id": "RTR7110",
                    "value": "000000000"
                },
                {
                    "id": "RTR7140",
                    "value": "000000000"
                },
                {
                    "id": "RTR7150",
                    "value": "000000995"
                },
                {
                    "id": "RTR7160",
                    "value": "000000996"
                },
                {
                    "id": "RTR7216",
                    "value": "000000000"
                },
                {
                    "id": "RTR7228",
                    "value": "000000994"
                },
                {
                    "id": "RTR7610",
                    "value": "000000995"
                },
                {
                    "id": "RTR7620",
                    "value": "000000995"
                },
                {
                    "id": "RTR8120",
                    "value": "000000149"
                },
                {
                    "id": "RTR8220",
                    "value": "000000149"
                },
                {
                    "id": "RTR8320",
                    "value": "000000149"
                },
                {
                    "id": "STU0300",
                    "value": "000000000"
                },
                {
                    "id": "STU0336",
                    "value": "000000098"
                },
                {
                    "id": "STU0337",
                    "value": "000000098"
                },
                {
                    "id": "STU0416",
                    "value": "000000098"
                },
                {
                    "id": "STU0436",
                    "value": "000000098"
                },
                {
                    "id": "STU0437",
                    "value": "000000098"
                },
                {
                    "id": "STU0438",
                    "value": "000000098"
                },
                {
                    "id": "STU0700",
                    "value": "000000098"
                },
                {
                    "id": "STU0701",
                    "value": "000000098"
                },
                {
                    "id": "STU0802",
                    "value": "000000098"
                },
                {
                    "id": "STU0806",
                    "value": "000000098"
                },
                {
                    "id": "STU0807",
                    "value": "000000098"
                },
                {
                    "id": "STU0812",
                    "value": "000000098"
                },
                {
                    "id": "STU0837",
                    "value": "000000098"
                },
                {
                    "id": "STU1100",
                    "value": "000000098"
                },
                {
                    "id": "STU1300",
                    "value": "000000098"
                },
                {
                    "id": "STU2000",
                    "value": "000000098"
                },
                {
                    "id": "STU2007",
                    "value": "000000098"
                },
                {
                    "id": "STU2550",
                    "value": "000000098"
                },
                {
                    "id": "STU2558",
                    "value": "000000098"
                },
                {
                    "id": "STU2580",
                    "value": "000000098"
                },
                {
                    "id": "STU2588",
                    "value": "000000098"
                },
                {
                    "id": "STU4180",
                    "value": "000000998"
                },
                {
                    "id": "STU5020",
                    "value": "999999998"
                },
                {
                    "id": "STU5031",
                    "value": "999999998"
                },
                {
                    "id": "STU5092",
                    "value": "999999998"
                },
                {
                    "id": "STU5120",
                    "value": "999999998"
                },
                {
                    "id": "STU5123",
                    "value": "999999998"
                },
                {
                    "id": "STU5127",
                    "value": "999999998"
                },
                {
                    "id": "STU5320",
                    "value": "999999998"
                },
                {
                    "id": "STU5327",
                    "value": "999999998"
                },
                {
                    "id": "STU5420",
                    "value": "999999998"
                },
                {
                    "id": "STU5424",
                    "value": "999999998"
                },
                {
                    "id": "STU5820",
                    "value": "999999998"
                },
                {
                    "id": "STU6200",
                    "value": "000000998"
                },
                {
                    "id": "STU6280",
                    "value": "000000998"
                },
                {
                    "id": "STU7110",
                    "value": "000000998"
                },
                {
                    "id": "STU7118",
                    "value": "000000998"
                },
                {
                    "id": "STU8120",
                    "value": "000009998"
                },
                {
                    "id": "STU8122",
                    "value": "000009998"
                },
                {
                    "id": "STU8125",
                    "value": "000009998"
                },
                {
                    "id": "STU8132",
                    "value": "000099998"
                },
                {
                    "id": "STU8142",
                    "value": "000009998"
                },
                {
                    "id": "STU8151",
                    "value": "000009998"
                },
                {
                    "id": "STU8220",
                    "value": "000009998"
                },
                {
                    "id": "STU8228",
                    "value": "000009998"
                },
                {
                    "id": "STU8320",
                    "value": "000009998"
                },
                {
                    "id": "USE0300",
                    "value": "000000000"
                },
                {
                    "id": "USE0416",
                    "value": "000000098"
                },
                {
                    "id": "USE5030",
                    "value": "999999998"
                },
                {
                    "id": "USE5320",
                    "value": "999999998"
                },
                {
                    "id": "USE8220",
                    "value": "000009998"
                },
                {
                    "id": "UTI0300",
                    "value": "000000000"
                },
                {
                    "id": "UTI0436",
                    "value": "000000098"
                },
                {
                    "id": "UTI2388",
                    "value": "000000098"
                },
                {
                    "id": "UTI4180",
                    "value": "000000998"
                },
                {
                    "id": "UTI5030",
                    "value": "999999998"
                },
                {
                    "id": "UTI6200",
                    "value": "000000998"
                },
                {
                    "id": "UTI6280",
                    "value": "000000998"
                },
                {
                    "id": "UTI8151",
                    "value": "000009998"
                },
                {
                    "id": "UTI8320",
                    "value": "000009998"
                }
            ],
            "trendedAttributes": [
                {
                    "id": "TAMP2701",
                    "value": "0999999997"
                },
                {
                    "id": "TAMP2702",
                    "value": "0999999997"
                },
                {
                    "id": "TAMP2703",
                    "value": "0999999997"
                },
                {
                    "id": "TAMP3701",
                    "value": "0999999997"
                },
                {
                    "id": "TAMP3702",
                    "value": "0999999997"
                },
                {
                    "id": "TAMP3703",
                    "value": "0999999997"
                },
                {
                    "id": "TAMP4710",
                    "value": "0000000097"
                },
                {
                    "id": "TAUA0451",
                    "value": "0000000098"
                },
                {
                    "id": "TAUA0452",
                    "value": "0999999998"
                },
                {
                    "id": "TAUA0453",
                    "value": "0000000098"
                },
                {
                    "id": "TAUA0454",
                    "value": "0999999998"
                },
                {
                    "id": "TAUA2701",
                    "value": "0999999998"
                },
                {
                    "id": "TAUA2702",
                    "value": "0999999998"
                },
                {
                    "id": "TAUA2703",
                    "value": "0999999998"
                },
                {
                    "id": "TAUA2711",
                    "value": "0999999998"
                },
                {
                    "id": "TAUA2712",
                    "value": "0000000098"
                },
                {
                    "id": "TAUA3701",
                    "value": "0999999998"
                },
                {
                    "id": "TAUA3702",
                    "value": "0999999998"
                },
                {
                    "id": "TAUA3703",
                    "value": "0999999998"
                },
                {
                    "id": "TAUA3711",
                    "value": "0999999998"
                },
                {
                    "id": "TAUA3712",
                    "value": "0000000098"
                },
                {
                    "id": "TAUA4708",
                    "value": "0999999998"
                },
                {
                    "id": "TAUA4709",
                    "value": "0000000098"
                },
                {
                    "id": "TAUA4710",
                    "value": "0000000098"
                },
                {
                    "id": "TAUT0901",
                    "value": "0000000098"
                },
                {
                    "id": "TAUT0904",
                    "value": "0000000098"
                },
                {
                    "id": "TAUT0905",
                    "value": "0999999998"
                },
                {
                    "id": "TAUT2901",
                    "value": "0000000098"
                },
                {
                    "id": "TAUT2902",
                    "value": "0999999998"
                },
                {
                    "id": "TAUT2903",
                    "value": "0999999998"
                },
                {
                    "id": "TAUT3901",
                    "value": "0000000098"
                },
                {
                    "id": "TAUT3902",
                    "value": "0999999998"
                },
                {
                    "id": "TAUT3903",
                    "value": "0999999998"
                },
                {
                    "id": "TAUT4901",
                    "value": "0000000098"
                },
                {
                    "id": "TAUT4902",
                    "value": "0999999998"
                },
                {
                    "id": "TAUT4903",
                    "value": "0999999998"
                },
                {
                    "id": "TBCA0451",
                    "value": "0000000097"
                },
                {
                    "id": "TBCA0452",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA0453",
                    "value": "0000000097"
                },
                {
                    "id": "TBCA0454",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA0455",
                    "value": "0000000097"
                },
                {
                    "id": "TBCA0456",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA0457",
                    "value": "0000000097"
                },
                {
                    "id": "TBCA0458",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA0904",
                    "value": "0000000095"
                },
                {
                    "id": "TBCA0906",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA0909",
                    "value": "0000000095"
                },
                {
                    "id": "TBCA0910",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA0963",
                    "value": "0000000095"
                },
                {
                    "id": "TBCA0964",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2101",
                    "value": "0000000001"
                },
                {
                    "id": "TBCA2105",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2106",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2107",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2108",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2109",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2201",
                    "value": "0000000163"
                },
                {
                    "id": "TBCA2251",
                    "value": "0000000248"
                },
                {
                    "id": "TBCA2252",
                    "value": "0000000248"
                },
                {
                    "id": "TBCA2253",
                    "value": "0000000248"
                },
                {
                    "id": "TBCA2255",
                    "value": "0000000119"
                },
                {
                    "id": "TBCA2256",
                    "value": "0000000036"
                },
                {
                    "id": "TBCA2257",
                    "value": "0000000036"
                },
                {
                    "id": "TBCA2260",
                    "value": "0000000148"
                },
                {
                    "id": "TBCA2263",
                    "value": "0000000003"
                },
                {
                    "id": "TBCA2264",
                    "value": "0000000004"
                },
                {
                    "id": "TBCA2265",
                    "value": "0000000002"
                },
                {
                    "id": "TBCA2266",
                    "value": "0000000004"
                },
                {
                    "id": "TBCA2267",
                    "value": "0000000212"
                },
                {
                    "id": "TBCA2271",
                    "value": "0000000004"
                },
                {
                    "id": "TBCA2273",
                    "value": "0000000119"
                },
                {
                    "id": "TBCA2274",
                    "value": "0000000148"
                },
                {
                    "id": "TBCA2275",
                    "value": "0000000148"
                },
                {
                    "id": "TBCA2276",
                    "value": "0000000003"
                },
                {
                    "id": "TBCA2277",
                    "value": "0000000004"
                },
                {
                    "id": "TBCA2278",
                    "value": "0000000004"
                },
                {
                    "id": "TBCA2279",
                    "value": "0000000163"
                },
                {
                    "id": "TBCA2282",
                    "value": "0000000996"
                },
                {
                    "id": "TBCA2283",
                    "value": "0000000100"
                },
                {
                    "id": "TBCA2301",
                    "value": "0000000001"
                },
                {
                    "id": "TBCA2358",
                    "value": "0000000212"
                },
                {
                    "id": "TBCA2379",
                    "value": "0000000001"
                },
                {
                    "id": "TBCA2526",
                    "value": "0000013337"
                },
                {
                    "id": "TBCA2527",
                    "value": "0000013337"
                },
                {
                    "id": "TBCA2601",
                    "value": "0000000012"
                },
                {
                    "id": "TBCA2602",
                    "value": "0000000184"
                },
                {
                    "id": "TBCA2603",
                    "value": "0000000073"
                },
                {
                    "id": "TBCA2604",
                    "value": "0000000064"
                },
                {
                    "id": "TBCA2606",
                    "value": "0000000012"
                },
                {
                    "id": "TBCA2607",
                    "value": "0000000184"
                },
                {
                    "id": "TBCA2608",
                    "value": "0000000073"
                },
                {
                    "id": "TBCA2609",
                    "value": "0000000064"
                },
                {
                    "id": "TBCA2610",
                    "value": "0000000009"
                },
                {
                    "id": "TBCA2611",
                    "value": "0000000196"
                },
                {
                    "id": "TBCA2612",
                    "value": "0000000196"
                },
                {
                    "id": "TBCA2613",
                    "value": "0000013304"
                },
                {
                    "id": "TBCA2614",
                    "value": "0000013304"
                },
                {
                    "id": "TBCA2615",
                    "value": "0000000001"
                },
                {
                    "id": "TBCA2616",
                    "value": "0000000001"
                },
                {
                    "id": "TBCA2617",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2618",
                    "value": "0000000001"
                },
                {
                    "id": "TBCA2619",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2620",
                    "value": "0000000100"
                },
                {
                    "id": "TBCA2622",
                    "value": "0000000152"
                },
                {
                    "id": "TBCA2623",
                    "value": "0000000010"
                },
                {
                    "id": "TBCA2624",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2625",
                    "value": "0000000001"
                },
                {
                    "id": "TBCA2626",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA2627",
                    "value": "0000000163"
                },
                {
                    "id": "TBCA2628",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2629",
                    "value": "0000000100"
                },
                {
                    "id": "TBCA2631",
                    "value": "0000000093"
                },
                {
                    "id": "TBCA2901",
                    "value": "0000000097"
                },
                {
                    "id": "TBCA2902",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA2903",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA2906",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2907",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2908",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2960",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2961",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA2962",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3101",
                    "value": "0000000001"
                },
                {
                    "id": "TBCA3105",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3201",
                    "value": "0000000130"
                },
                {
                    "id": "TBCA3251",
                    "value": "0000000248"
                },
                {
                    "id": "TBCA3252",
                    "value": "0000000248"
                },
                {
                    "id": "TBCA3253",
                    "value": "0000000248"
                },
                {
                    "id": "TBCA3255",
                    "value": "0000000119"
                },
                {
                    "id": "TBCA3256",
                    "value": "0000000036"
                },
                {
                    "id": "TBCA3257",
                    "value": "0000000036"
                },
                {
                    "id": "TBCA3260",
                    "value": "0000000148"
                },
                {
                    "id": "TBCA3263",
                    "value": "0000000003"
                },
                {
                    "id": "TBCA3264",
                    "value": "0000000004"
                },
                {
                    "id": "TBCA3265",
                    "value": "0000000002"
                },
                {
                    "id": "TBCA3266",
                    "value": "0000000004"
                },
                {
                    "id": "TBCA3267",
                    "value": "0000000212"
                },
                {
                    "id": "TBCA3269",
                    "value": "0000000002"
                },
                {
                    "id": "TBCA3270",
                    "value": "0000000002"
                },
                {
                    "id": "TBCA3271",
                    "value": "0000000004"
                },
                {
                    "id": "TBCA3273",
                    "value": "0000000119"
                },
                {
                    "id": "TBCA3274",
                    "value": "0000000148"
                },
                {
                    "id": "TBCA3275",
                    "value": "0000000148"
                },
                {
                    "id": "TBCA3276",
                    "value": "0000000003"
                },
                {
                    "id": "TBCA3277",
                    "value": "0000000004"
                },
                {
                    "id": "TBCA3278",
                    "value": "0000000004"
                },
                {
                    "id": "TBCA3279",
                    "value": "0000000130"
                },
                {
                    "id": "TBCA3282",
                    "value": "0000000996"
                },
                {
                    "id": "TBCA3283",
                    "value": "0000000100"
                },
                {
                    "id": "TBCA3301",
                    "value": "0000000001"
                },
                {
                    "id": "TBCA3358",
                    "value": "0000000212"
                },
                {
                    "id": "TBCA3379",
                    "value": "0000000001"
                },
                {
                    "id": "TBCA3401",
                    "value": "0000000096"
                },
                {
                    "id": "TBCA3402",
                    "value": "0999999996"
                },
                {
                    "id": "TBCA3403",
                    "value": "0000000096"
                },
                {
                    "id": "TBCA3404",
                    "value": "0999999996"
                },
                {
                    "id": "TBCA3526",
                    "value": "0000013370"
                },
                {
                    "id": "TBCA3527",
                    "value": "0000013370"
                },
                {
                    "id": "TBCA3601",
                    "value": "0000000006"
                },
                {
                    "id": "TBCA3602",
                    "value": "0000000167"
                },
                {
                    "id": "TBCA3603",
                    "value": "0000000059"
                },
                {
                    "id": "TBCA3604",
                    "value": "0000000052"
                },
                {
                    "id": "TBCA3606",
                    "value": "0000000006"
                },
                {
                    "id": "TBCA3607",
                    "value": "0000000167"
                },
                {
                    "id": "TBCA3608",
                    "value": "0000000059"
                },
                {
                    "id": "TBCA3609",
                    "value": "0000000052"
                },
                {
                    "id": "TBCA3610",
                    "value": "0000000015"
                },
                {
                    "id": "TBCA3617",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3618",
                    "value": "0000000001"
                },
                {
                    "id": "TBCA3619",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3620",
                    "value": "0000000100"
                },
                {
                    "id": "TBCA3622",
                    "value": "0000000116"
                },
                {
                    "id": "TBCA3623",
                    "value": "0000000010"
                },
                {
                    "id": "TBCA3624",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3625",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3626",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA3627",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA3628",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3629",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3631",
                    "value": "0000000089"
                },
                {
                    "id": "TBCA3901",
                    "value": "0000000097"
                },
                {
                    "id": "TBCA3902",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA3903",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA3906",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3907",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3908",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3960",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3961",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA3962",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA4103",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA4104",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA4280",
                    "value": "0000000004"
                },
                {
                    "id": "TBCA4281",
                    "value": "0000000004"
                },
                {
                    "id": "TBCA4901",
                    "value": "0000000097"
                },
                {
                    "id": "TBCA4902",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA4903",
                    "value": "0999999997"
                },
                {
                    "id": "TBCA4906",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA4907",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA4908",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA4960",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA4961",
                    "value": "0000000000"
                },
                {
                    "id": "TBCA4962",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1201",
                    "value": "0000000202"
                },
                {
                    "id": "TBCC1203",
                    "value": "0999999997"
                },
                {
                    "id": "TBCC1204",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC1205",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1206",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC1207",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC1208",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1209",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1210",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1211",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1212",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1213",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1261",
                    "value": "0000000065"
                },
                {
                    "id": "TBCC1301",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC1303",
                    "value": "0000000997"
                },
                {
                    "id": "TBCC1304",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC1305",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1306",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1307",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1308",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1309",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1310",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1311",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1312",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1313",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1314",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC1501",
                    "value": "0999999997"
                },
                {
                    "id": "TBCC1502",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1503",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC1751",
                    "value": "0000000010"
                },
                {
                    "id": "TBCC1752",
                    "value": "0000000010"
                },
                {
                    "id": "TBCC1753",
                    "value": "0000000155"
                },
                {
                    "id": "TBCC2101",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC2102",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2201",
                    "value": "0000000163"
                },
                {
                    "id": "TBCC2203",
                    "value": "0999999997"
                },
                {
                    "id": "TBCC2204",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC2205",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC2206",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC2207",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC2208",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC2209",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC2210",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC2211",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC2212",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC2213",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2251",
                    "value": "0000000248"
                },
                {
                    "id": "TBCC2256",
                    "value": "0000000036"
                },
                {
                    "id": "TBCC2261",
                    "value": "0000000060"
                },
                {
                    "id": "TBCC2262",
                    "value": "0000000097"
                },
                {
                    "id": "TBCC2301",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC2303",
                    "value": "0000000997"
                },
                {
                    "id": "TBCC2304",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC2305",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC2306",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2307",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2308",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2309",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2310",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2311",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2312",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2313",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2314",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2351",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC2356",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2362",
                    "value": "0000000097"
                },
                {
                    "id": "TBCC2365",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC2366",
                    "value": "0000000004"
                },
                {
                    "id": "TBCC2501",
                    "value": "0999999997"
                },
                {
                    "id": "TBCC2502",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2503",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC2751",
                    "value": "0000000010"
                },
                {
                    "id": "TBCC2752",
                    "value": "0000000010"
                },
                {
                    "id": "TBCC2753",
                    "value": "0000000155"
                },
                {
                    "id": "TBCC3101",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC3102",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3201",
                    "value": "0000000130"
                },
                {
                    "id": "TBCC3203",
                    "value": "0999999997"
                },
                {
                    "id": "TBCC3204",
                    "value": "0000000006"
                },
                {
                    "id": "TBCC3205",
                    "value": "0000000003"
                },
                {
                    "id": "TBCC3206",
                    "value": "0000000005"
                },
                {
                    "id": "TBCC3207",
                    "value": "0000000004"
                },
                {
                    "id": "TBCC3208",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC3209",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC3210",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC3211",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC3212",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC3213",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3251",
                    "value": "0000000248"
                },
                {
                    "id": "TBCC3256",
                    "value": "0000000036"
                },
                {
                    "id": "TBCC3261",
                    "value": "-000000090"
                },
                {
                    "id": "TBCC3262",
                    "value": "0000000097"
                },
                {
                    "id": "TBCC3301",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC3303",
                    "value": "0000000997"
                },
                {
                    "id": "TBCC3304",
                    "value": "0000000003"
                },
                {
                    "id": "TBCC3305",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC3306",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3307",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3308",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3309",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3310",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3311",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3312",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3313",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3314",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3351",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC3356",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3362",
                    "value": "0000000097"
                },
                {
                    "id": "TBCC3365",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC3366",
                    "value": "0000000004"
                },
                {
                    "id": "TBCC3501",
                    "value": "0999999997"
                },
                {
                    "id": "TBCC3502",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3503",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC3751",
                    "value": "0000000010"
                },
                {
                    "id": "TBCC3752",
                    "value": "0000000010"
                },
                {
                    "id": "TBCC3753",
                    "value": "0000000155"
                },
                {
                    "id": "TBCC4201",
                    "value": "0000000177"
                },
                {
                    "id": "TBCC4203",
                    "value": "0999999997"
                },
                {
                    "id": "TBCC4204",
                    "value": "0000000012"
                },
                {
                    "id": "TBCC4205",
                    "value": "0000000009"
                },
                {
                    "id": "TBCC4206",
                    "value": "0000000011"
                },
                {
                    "id": "TBCC4207",
                    "value": "0000000009"
                },
                {
                    "id": "TBCC4208",
                    "value": "0000000007"
                },
                {
                    "id": "TBCC4209",
                    "value": "0000000005"
                },
                {
                    "id": "TBCC4210",
                    "value": "0000000008"
                },
                {
                    "id": "TBCC4211",
                    "value": "0000000006"
                },
                {
                    "id": "TBCC4212",
                    "value": "0000000002"
                },
                {
                    "id": "TBCC4213",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC4255",
                    "value": "0000000366"
                },
                {
                    "id": "TBCC4260",
                    "value": "-000000375"
                },
                {
                    "id": "TBCC4263",
                    "value": "0000000022"
                },
                {
                    "id": "TBCC4264",
                    "value": "0000000021"
                },
                {
                    "id": "TBCC4301",
                    "value": "0000000001"
                },
                {
                    "id": "TBCC4303",
                    "value": "0000000997"
                },
                {
                    "id": "TBCC4304",
                    "value": "0000000008"
                },
                {
                    "id": "TBCC4305",
                    "value": "0000000007"
                },
                {
                    "id": "TBCC4306",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC4307",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC4308",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC4309",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC4310",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC4311",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC4312",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC4313",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC4314",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC4355",
                    "value": "0000000003"
                },
                {
                    "id": "TBCC4360",
                    "value": "000000-003"
                },
                {
                    "id": "TBCC4363",
                    "value": "0000000022"
                },
                {
                    "id": "TBCC4364",
                    "value": "0000000021"
                },
                {
                    "id": "TBCC4501",
                    "value": "0999999997"
                },
                {
                    "id": "TBCC4502",
                    "value": "0000000000"
                },
                {
                    "id": "TBCC4503",
                    "value": "0000000000"
                },
                {
                    "id": "TCOL2551",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2552",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2554",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2555",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2556",
                    "value": "0000000098"
                },
                {
                    "id": "TCOL2557",
                    "value": "0000000098"
                },
                {
                    "id": "TCOL2561",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2562",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2564",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2565",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2566",
                    "value": "0000000098"
                },
                {
                    "id": "TCOL2567",
                    "value": "0000000098"
                },
                {
                    "id": "TCOL2571",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2572",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2581",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2582",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2591",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL2592",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3551",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3552",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3553",
                    "value": "0000000098"
                },
                {
                    "id": "TCOL3554",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3555",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3556",
                    "value": "0000000098"
                },
                {
                    "id": "TCOL3557",
                    "value": "0000000098"
                },
                {
                    "id": "TCOL3561",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3562",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3563",
                    "value": "0000000098"
                },
                {
                    "id": "TCOL3564",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3565",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3566",
                    "value": "0000000098"
                },
                {
                    "id": "TCOL3567",
                    "value": "0000000098"
                },
                {
                    "id": "TCOL3571",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3572",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3573",
                    "value": "0000000098"
                },
                {
                    "id": "TCOL3581",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3582",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3583",
                    "value": "0000000098"
                },
                {
                    "id": "TCOL3591",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3592",
                    "value": "0999999998"
                },
                {
                    "id": "TCOL3593",
                    "value": "0000000098"
                },
                {
                    "id": "THLC1201",
                    "value": "0999999998"
                },
                {
                    "id": "THLC1203",
                    "value": "0999999998"
                },
                {
                    "id": "THLC1261",
                    "value": "0999999998"
                },
                {
                    "id": "THLC1501",
                    "value": "0999999998"
                },
                {
                    "id": "THLC1502",
                    "value": "0000009998"
                },
                {
                    "id": "THLC1503",
                    "value": "0000000098"
                },
                {
                    "id": "THLC1751",
                    "value": "0999999998"
                },
                {
                    "id": "THLC1752",
                    "value": "0999999998"
                },
                {
                    "id": "THLC1753",
                    "value": "0999999998"
                },
                {
                    "id": "THLC2201",
                    "value": "0999999998"
                },
                {
                    "id": "THLC2203",
                    "value": "0999999998"
                },
                {
                    "id": "THLC2261",
                    "value": "0999999998"
                },
                {
                    "id": "THLC2501",
                    "value": "0999999998"
                },
                {
                    "id": "THLC2502",
                    "value": "0000009998"
                },
                {
                    "id": "THLC2503",
                    "value": "0000000098"
                },
                {
                    "id": "THLC2751",
                    "value": "0999999998"
                },
                {
                    "id": "THLC2752",
                    "value": "0999999998"
                },
                {
                    "id": "THLC2753",
                    "value": "0999999998"
                },
                {
                    "id": "THLC3201",
                    "value": "0999999998"
                },
                {
                    "id": "THLC3203",
                    "value": "0999999998"
                },
                {
                    "id": "THLC3261",
                    "value": "0999999998"
                },
                {
                    "id": "THLC3501",
                    "value": "0999999998"
                },
                {
                    "id": "THLC3502",
                    "value": "0000009998"
                },
                {
                    "id": "THLC3503",
                    "value": "0000000098"
                },
                {
                    "id": "THLC3751",
                    "value": "0999999998"
                },
                {
                    "id": "THLC3752",
                    "value": "0999999998"
                },
                {
                    "id": "THLC3753",
                    "value": "0999999998"
                },
                {
                    "id": "THLC4201",
                    "value": "0999999998"
                },
                {
                    "id": "THLC4203",
                    "value": "0999999998"
                },
                {
                    "id": "THLC4501",
                    "value": "0999999998"
                },
                {
                    "id": "THLC4502",
                    "value": "0000009998"
                },
                {
                    "id": "THLC4503",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI0451",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI0452",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI0453",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI0454",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI0455",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI0456",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI0457",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI0458",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI0901",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI0904",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI0905",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI2701",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI2702",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI2703",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI2711",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI2712",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI2901",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI2902",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI2903",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI3701",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI3702",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI3703",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI3711",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI3712",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI3901",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI3902",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI3903",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI4708",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI4709",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI4710",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI4901",
                    "value": "0000000098"
                },
                {
                    "id": "TMTI4902",
                    "value": "0999999998"
                },
                {
                    "id": "TMTI4903",
                    "value": "0999999998"
                },
                {
                    "id": "TPIL0901",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL0904",
                    "value": "0000000095"
                },
                {
                    "id": "TPIL0905",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL0921",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL0924",
                    "value": "0000000095"
                },
                {
                    "id": "TPIL0925",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL0983",
                    "value": "0000000095"
                },
                {
                    "id": "TPIL0984",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL2701",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL2702",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL2703",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL2711",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL2712",
                    "value": "0000000097"
                },
                {
                    "id": "TPIL2901",
                    "value": "0000000097"
                },
                {
                    "id": "TPIL2902",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL2903",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL2921",
                    "value": "0000000097"
                },
                {
                    "id": "TPIL2922",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL2923",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL2980",
                    "value": "0000000097"
                },
                {
                    "id": "TPIL2981",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL2982",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL3701",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL3702",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL3703",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL3711",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL3712",
                    "value": "0000000097"
                },
                {
                    "id": "TPIL3901",
                    "value": "0000000097"
                },
                {
                    "id": "TPIL3902",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL3903",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL3921",
                    "value": "0000000097"
                },
                {
                    "id": "TPIL3922",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL3923",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL3980",
                    "value": "0000000097"
                },
                {
                    "id": "TPIL3981",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL3982",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL4708",
                    "value": "0999999997"
                },
                {
                    "id": "TPIL4709",
                    "value": "0000000097"
                },
                {
                    "id": "TPIL4710",
                    "value": "0000000097"
                },
                {
                    "id": "TPIL4901",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL4902",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL4903",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL4921",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL4922",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL4923",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL4980",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL4981",
                    "value": "0000000000"
                },
                {
                    "id": "TPIL4982",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR1201",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR1203",
                    "value": "0999999997"
                },
                {
                    "id": "TRTR1261",
                    "value": "-000000167"
                },
                {
                    "id": "TRTR1501",
                    "value": "0999999997"
                },
                {
                    "id": "TRTR1502",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR1503",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR1751",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR1752",
                    "value": "0999999996"
                },
                {
                    "id": "TRTR1753",
                    "value": "0999999996"
                },
                {
                    "id": "TRTR2101",
                    "value": "0000000001"
                },
                {
                    "id": "TRTR2102",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR2201",
                    "value": "0000000100"
                },
                {
                    "id": "TRTR2203",
                    "value": "0999999997"
                },
                {
                    "id": "TRTR2261",
                    "value": "-000001400"
                },
                {
                    "id": "TRTR2301",
                    "value": "0000000001"
                },
                {
                    "id": "TRTR2501",
                    "value": "0999999997"
                },
                {
                    "id": "TRTR2502",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR2503",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR2601",
                    "value": "-000000186"
                },
                {
                    "id": "TRTR2602",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR2603",
                    "value": "0000000418"
                },
                {
                    "id": "TRTR2604",
                    "value": "0000000213"
                },
                {
                    "id": "TRTR2610",
                    "value": "0000000069"
                },
                {
                    "id": "TRTR2611",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR2613",
                    "value": "0000007500"
                },
                {
                    "id": "TRTR2615",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR2617",
                    "value": "0000000001"
                },
                {
                    "id": "TRTR2618",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR2751",
                    "value": "0999999996"
                },
                {
                    "id": "TRTR2752",
                    "value": "0999999996"
                },
                {
                    "id": "TRTR2753",
                    "value": "0999999996"
                },
                {
                    "id": "TRTR3101",
                    "value": "0000000001"
                },
                {
                    "id": "TRTR3102",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR3105",
                    "value": "0000000001"
                },
                {
                    "id": "TRTR3106",
                    "value": "0000000001"
                },
                {
                    "id": "TRTR3107",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR3201",
                    "value": "0000000864"
                },
                {
                    "id": "TRTR3203",
                    "value": "0999999997"
                },
                {
                    "id": "TRTR3251",
                    "value": "0000001900"
                },
                {
                    "id": "TRTR3252",
                    "value": "0000001900"
                },
                {
                    "id": "TRTR3253",
                    "value": "0000001900"
                },
                {
                    "id": "TRTR3255",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR3256",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR3257",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR3260",
                    "value": "0000000600"
                },
                {
                    "id": "TRTR3261",
                    "value": "-000001395"
                },
                {
                    "id": "TRTR3264",
                    "value": "0000000008"
                },
                {
                    "id": "TRTR3265",
                    "value": "0000000011"
                },
                {
                    "id": "TRTR3267",
                    "value": "0000001900"
                },
                {
                    "id": "TRTR3269",
                    "value": "0000000011"
                },
                {
                    "id": "TRTR3274",
                    "value": "0000000600"
                },
                {
                    "id": "TRTR3277",
                    "value": "0000000008"
                },
                {
                    "id": "TRTR3282",
                    "value": "0000000996"
                },
                {
                    "id": "TRTR3301",
                    "value": "0000000012"
                },
                {
                    "id": "TRTR3358",
                    "value": "0000001900"
                },
                {
                    "id": "TRTR3401",
                    "value": "0000000096"
                },
                {
                    "id": "TRTR3402",
                    "value": "0999999996"
                },
                {
                    "id": "TRTR3403",
                    "value": "0000000096"
                },
                {
                    "id": "TRTR3404",
                    "value": "0999999996"
                },
                {
                    "id": "TRTR3501",
                    "value": "0999999997"
                },
                {
                    "id": "TRTR3502",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR3503",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR3526",
                    "value": "0000006636"
                },
                {
                    "id": "TRTR3601",
                    "value": "-000000233"
                },
                {
                    "id": "TRTR3602",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR3603",
                    "value": "0000000862"
                },
                {
                    "id": "TRTR3604",
                    "value": "0000000189"
                },
                {
                    "id": "TRTR3610",
                    "value": "0000000095"
                },
                {
                    "id": "TRTR3617",
                    "value": "0000000001"
                },
                {
                    "id": "TRTR3618",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR3619",
                    "value": "0000000100"
                },
                {
                    "id": "TRTR3620",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR3622",
                    "value": "0000000259"
                },
                {
                    "id": "TRTR3623",
                    "value": "0999999996"
                },
                {
                    "id": "TRTR3624",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR3625",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR3751",
                    "value": "0999999996"
                },
                {
                    "id": "TRTR3752",
                    "value": "0999999996"
                },
                {
                    "id": "TRTR3753",
                    "value": "0999999996"
                },
                {
                    "id": "TRTR4103",
                    "value": "0000000004"
                },
                {
                    "id": "TRTR4104",
                    "value": "0000000004"
                },
                {
                    "id": "TRTR4201",
                    "value": "0000000980"
                },
                {
                    "id": "TRTR4203",
                    "value": "0999999997"
                },
                {
                    "id": "TRTR4280",
                    "value": "0000000005"
                },
                {
                    "id": "TRTR4281",
                    "value": "0000000005"
                },
                {
                    "id": "TRTR4501",
                    "value": "0999999997"
                },
                {
                    "id": "TRTR4502",
                    "value": "0000000000"
                },
                {
                    "id": "TRTR4503",
                    "value": "0000000000"
                },
                {
                    "id": "TSTU0906",
                    "value": "0000000098"
                },
                {
                    "id": "TSTU0909",
                    "value": "0000000098"
                },
                {
                    "id": "TSTU0910",
                    "value": "0999999998"
                },
                {
                    "id": "TSTU2906",
                    "value": "0000000098"
                },
                {
                    "id": "TSTU2907",
                    "value": "0999999998"
                },
                {
                    "id": "TSTU2908",
                    "value": "0999999998"
                },
                {
                    "id": "TSTU3906",
                    "value": "0000000098"
                },
                {
                    "id": "TSTU3907",
                    "value": "0999999998"
                },
                {
                    "id": "TSTU3908",
                    "value": "0999999998"
                },
                {
                    "id": "TSTU4906",
                    "value": "0000000098"
                },
                {
                    "id": "TSTU4907",
                    "value": "0999999998"
                },
                {
                    "id": "TSTU4908",
                    "value": "0999999998"
                }
            ],
            "endTotals": [
                {
                    "totalSegments": "044",
                    "totalLength": "40888"
                }
            ]
        }
    ]
}

|]
