{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Test.Underwrite.Clarity.Samples where

import Data.ByteString.Lazy (ByteString)
import Data.String.Here


request :: ByteString
request = [here|
<?xml version="1.0" encoding="UTF-8"?>
<inquiry>
    <group-id>101</group-id>
    <account-id>201</account-id>
    <location-id>8642</location-id>
    <username>username</username>
    <password>password</password>
    <control-file-name>Test_TimelyAdvances</control-file-name>
    <control-file-version-number></control-file-version-number>
    <inquiry-purpose-type>AR</inquiry-purpose-type>
    <inquiry-tradeline-type>C7</inquiry-tradeline-type>
    <first-name>John</first-name>
    <last-name>Fence</last-name>
    <social-security-number>301001240</social-security-number>
    <date-of-birth>1996-03-06</date-of-birth>
    <drivers-license-number>193444830</drivers-license-number>
    <drivers-license-state>UT</drivers-license-state>
    <street-address-1>1234 W Avenue</street-address-1>
    <street-address-2/>
    <city>Salt Lake City</city>
    <state>UT</state>
    <zip-code>84108</zip-code>
    <email-address>john.fence@gmail.com</email-address>
    <home-phone>8015551234</home-phone>
    <cell-phone>8015551234</cell-phone>
    <bank-routing-number>314977188</bank-routing-number>
    <bank-account-number>1132243112544</bank-account-number>
    <bank-account-type>Checking</bank-account-type>
    <employer-name>Everest Auto</employer-name>
    <net-monthly-income>4200.00</net-monthly-income>
    <date-of-next-payday>2016-12-15</date-of-next-payday>
    <pay-frequency>SemiMonthly</pay-frequency>
    <loan-amount>1000.00</loan-amount>
</inquiry>
|]

response :: ByteString
response = [here|
<?xml version="1.0" encoding="UTF-8"?>
<xml-response>
    <tracking-number>vha5zkbmpz</tracking-number>
    <action>Approve</action>
    <deny-codes nil="true"/>
    <deny-descriptions nil="true"/>
    <exception-descriptions nil="true"/>
    <filter-codes nil="true"/>
    <filter-descriptions nil="true"/>
    <product-notices/>
    <consumer-alerts>
        <facta-alerts></facta-alerts>
        <non-facta-alerts></non-facta-alerts>
    </consumer-alerts>
    <clear-products-request>
        <username>timelyadvancetestutility</username>
        <control-file-substituted>Clear Control file version not specified; using most recent approved version.</control-file-substituted>
        <control-file-name>Test_TimelyAdvances</control-file-name>
        <control-file-version-number>1</control-file-version-number>
        <group-name>Tampa Check Cashing</group-name>
        <account-name>Customer and Product Testing Account</account-name>
        <location-name>Timely Advances Testing</location-name>
        <action>Approve</action>
        <deny-codes nil="true"/>
        <deny-descriptions nil="true"/>
        <exception-descriptions nil="true"/>
        <filter-codes nil="true"/>
        <filter-descriptions nil="true"/>
    </clear-products-request>
    <inquiry>
        <state>UT</state>
        <months-at-current-employer nil="true"/>
        <net-monthly-income>4200</net-monthly-income>
        <paycheck-direct-deposit nil="true"/>
        <date-of-next-payday>2016-12-15</date-of-next-payday>
        <last-name>FENCE</last-name>
        <first-name>JOHN</first-name>
        <middle-initial nil="true"/>
        <street-address-1>1234 W AVENUE</street-address-1>
        <street-address-2 nil="true"/>
        <date-of-birth>1996-03-06</date-of-birth>
        <city>SALT LAKE CITY</city>
        <zip-code>84108</zip-code>
        <housing-status nil="true"/>
        <months-at-address nil="true"/>
        <home-phone nil="true"/>
        <cell-phone nil="true"/>
        <drivers-license-state>UT</drivers-license-state>
        <bank-account-type>CHECKING</bank-account-type>
        <employer-name>EVEREST AUTO</employer-name>
        <employer-address nil="true"/>
        <employer-city nil="true"/>
        <employer-state nil="true"/>
        <occupation-type nil="true"/>
        <work-phone nil="true"/>
        <work-phone-extension nil="true"/>
        <work-fax-number nil="true"/>
        <pay-frequency>SEMIMONTHLY</pay-frequency>
        <reference-last-name nil="true"/>
        <reference-first-name nil="true"/>
        <reference-phone nil="true"/>
        <reference-relationship nil="true"/>
        <control-file-name>Test_TimelyAdvances</control-file-name>
        <control-file-version-number nil="true"/>
        <inquiry-received-at>2019-05-23T22:32:00Z</inquiry-received-at>
        <products-requested>Clear Fraud,Clear Bank Behavior,Clear Credit Risk,Clear Advanced Attributes,Clear Fraud Insight</products-requested>
        <products-executed>Clear Fraud, Clear Bank Behavior, Clear Credit Risk, Clear Advanced Attributes, Clear Fraud Insight</products-executed>
        <inquiry-purpose-type>AR</inquiry-purpose-type>
        <inquiry-tradeline-type>C7</inquiry-tradeline-type>
        <email-address>JOHN.FENCE@GMAIL.COM</email-address>
        <email-name nil="true"/>
        <email-domain-name nil="true"/>
        <origination-ip-address nil="true"/>
        <pc-digital-fingerprint nil="true"/>
        <pc-fingerprint-time-offset nil="true"/>
        <username>timelyadvancetestutility</username>
        <pcpsession nil="true"/>
        <pcpvendor nil="true"/>
        <social-security-valid>false</social-security-valid>
        <social-security-deceased>false</social-security-deceased>
        <bank-routing-valid>true</bank-routing-valid>
        <number-of-ssns-with-bank-account>1</number-of-ssns-with-bank-account>
        <drivers-license-number>193444830</drivers-license-number>
        <bank-routing-number>314977188</bank-routing-number>
        <debit-card-expiration nil="true"/>
        <tracking-number nil="true"/>
        <date-of-last-activity>2018-09-07T15:15:49Z</date-of-last-activity>
        <ofac-score>30</ofac-score>
        <check-amount nil="true"/>
        <check-serial-number nil="true"/>
        <check-issue-date nil="true"/>
        <check-type nil="true"/>
        <check-bank-routing-number nil="true"/>
        <pass-through-1 nil="true"/>
        <pass-through-2 nil="true"/>
        <pass-through-3 nil="true"/>
        <pass-through-4 nil="true"/>
        <pass-through-5 nil="true"/>
        <ofac-match>false</ofac-match>
        <last-seen-by-group nil="true"/>
        <last-seen-by-account nil="true"/>
        <last-seen-by-location nil="true"/>
        <last-purchased nil="true"/>
        <last-purchased-by-group nil="true"/>
        <requested-amount nil="true"/>
        <active-military nil="true"/>
        <loan-duration nil="true"/>
        <account-age nil="true"/>
        <action>Approve</action>
        <deny-codes nil="true"/>
        <deny-descriptions nil="true"/>
        <exception-descriptions nil="true"/>
        <server>louapp3</server>
        <vin nil="true"/>
        <bank-account-zeros>0</bank-account-zeros>
        <check-bank-account-zeros nil="true"/>
        <delivery-point nil="true"/>
        <leadgen nil="true"/>
        <drivers-license-invalid>false</drivers-license-invalid>
        <total-historical-inquiries>78</total-historical-inquiries>
        <ssn-first-appearance>false</ssn-first-appearance>
        <generational-code nil="true"/>
        <full-name>FENCE, JOHN </full-name>
        <product-date>2019-05-23T22:32:00Z</product-date>
        <filter-codes nil="true"/>
        <filter-descriptions nil="true"/>
        <social-security-number>301001240</social-security-number>
        <bank-account-number>1132243112544</bank-account-number>
        <check-bank-account-number nil="true"/>
        <ssn-first-name-count>12</ssn-first-name-count>
        <ssn-last-name-count>1</ssn-last-name-count>
        <ssn-first-last-name-count>1</ssn-first-last-name-count>
        <ssn-distinct-first-last-name-count>31</ssn-distinct-first-last-name-count>
        <debit-card-number nil="true"/>
        <initiating-inquiry nil="true"/>
    </inquiry>
    <clear-bank-behavior>
        <action>Approve</action>
        <deny-codes nil="true"/>
        <deny-descriptions nil="true"/>
        <exception-descriptions nil="true"/>
        <cbb-score>581</cbb-score>
        <cbb-reason-code-description>(BB109(*))|(BB108(*))|(BB103)|(BB110(*))</cbb-reason-code-description>
        <estimated-bank-history>477</estimated-bank-history>
        <number-of-accounts-all>7</number-of-accounts-all>
        <number-of-accounts-active>2</number-of-accounts-active>
        <number-of-accounts-with-check-history>3</number-of-accounts-with-check-history>
        <positive-check-writing-history>false</positive-check-writing-history>
        <check-cashing-history nil="true"/>
        <days-since-last-check-cashing-activity nil="true"/>
        <days-since-last-successful-check-cashed nil="true"/>
        <number-of-accounts-at-high-risk-banks>1</number-of-accounts-at-high-risk-banks>
        <number-of-high-risk-accounts>2</number-of-high-risk-accounts>
        <number-of-low-risk-accounts>2</number-of-low-risk-accounts>
        <number-of-unknown-risk-accounts>1</number-of-unknown-risk-accounts>
        <number-of-accounts-with-default-history>2</number-of-accounts-with-default-history>
        <number-of-accounts-linked-to-fraud>1</number-of-accounts-linked-to-fraud>
        <number-of-accounts-with-alternate-ssns>1</number-of-accounts-with-alternate-ssns>
        <max-number-of-ssns-with-micr>21</max-number-of-ssns-with-micr>
        <reason-code-description>(A01(#)) Primary account has retail check writing history|(A07) Primary account first seen by Clarity in 31-60 days|(A49) Primary account has an ACH return within 30 days|(A50) Primary account has an ACH return within 90 days|(A51) Primary account has an ACH return within 180 days|(A52) Primary account has an ACH return within 1 year|(B01(#)) Secondary account has retail check writing history|(B07) Secondary account first seen by Clarity in 31-60 days|(B11) Secondary account may be Fraudulent|(B01(#)) Secondary account has retail check writing history|(B08) Secondary account first seen by Clarity in 61-90 days|(B05) Secondary account ownership confirmed|(B05) Secondary account ownership confirmed</reason-code-description>
        <cbb-non-scorable-reason nil="true"/>
        <cbb-non-scorable-reason-description nil="true"/>
        <cbb-score2 nil="true"/>
        <cbb-reason-code-description2 nil="true"/>
        <full-name>FENCE, JOHN </full-name>
        <product-date>2019-05-23T22:32:00Z</product-date>
        <filter-codes nil="true"/>
        <filter-descriptions nil="true"/>
        <fis-chex-advisor>
            <consumer-privacy-message-text>NO CONSUMER PRIVACY DATA ON FILE</consumer-privacy-message-text>
            <error-details/>
            <avg-number-days-between-closure-5-years-ago>96</avg-number-days-between-closure-5-years-ago>
            <consumer-dispute-quantity-5-years-ago>0</consumer-dispute-quantity-5-years-ago>
            <consumer-dispute-resolved-quantity-5-years-ago>0</consumer-dispute-resolved-quantity-5-years-ago>
            <number-days-since-first-closure-5-years-ago>1465</number-days-since-first-closure-5-years-ago>
            <number-days-since-most-recent-closure-5-years-ago>25</number-days-since-most-recent-closure-5-years-ago>
            <no-closures>false</no-closures>
            <avg-number-of-checks-ordered>40</avg-number-of-checks-ordered>
            <number-of-days-since-first-order>1000</number-of-days-since-first-order>
            <number-of-days-since-most-recent-order>28</number-of-days-since-most-recent-order>
            <number-of-different-accounts>7</number-of-different-accounts>
            <consumer-statement-text1>Hey somebody stole my identity</consumer-statement-text1>
            <consumer-statement-text2>Hey this is the guy that stole his identity.  Don't pay any attention to him.</consumer-statement-text2>
            <consumer-statement-text3></consumer-statement-text3>
            <driver-license-validation-message-text>VALID DRIVERS LICENSE FORMAT</driver-license-validation-message-text>
            <government-number-validation-message-text>SSN AVAILABLE FOR RANDOMIZED ISSUANCE</government-number-validation-message-text>
            <debit-bureau-reason-codes>ED|EH|EL|EK</debit-bureau-reason-codes>
            <debit-bureau-reason-texts>DDA CLOSURE(S)|UNIQUE FI DDA INQUIRY HISTORY|RETAIL ITEM HISTORY|DDA CLOSURE PAYMENT HISTORY</debit-bureau-reason-texts>
            <debit-bureau-score>477</debit-bureau-score>
            <episode-span-all-item-total-number-of-days-3-years-ago>785</episode-span-all-item-total-number-of-days-3-years-ago>
            <max-amount-open-item-3-years-ago>60000</max-amount-open-item-3-years-ago>
            <max-amount-paid-item-3-years-ago>50000</max-amount-paid-item-3-years-ago>
            <max-number-days-to-pay-3-years-ago>370</max-number-days-to-pay-3-years-ago>
            <min-check-number-on-open-item-3-years-ago>1002</min-check-number-on-open-item-3-years-ago>
            <min-number-days-to-pay-3-years-ago>35</min-number-days-to-pay-3-years-ago>
            <number-days-most-recent-open-item-3-years-ago>15</number-days-most-recent-open-item-3-years-ago>
            <number-days-most-recent-paid-item-3-years-ago>45</number-days-most-recent-paid-item-3-years-ago>
            <no-returned-checks>false</no-returned-checks>
            <number-of-inquiries-in-3-years-ago-dda-and-non-dda>14</number-of-inquiries-in-3-years-ago-dda-and-non-dda>
            <no-previous-inquiry-dda-and-non-dda>false</no-previous-inquiry-dda-and-non-dda>
            <no-debit-data-found>false</no-debit-data-found>
            <number-of-days-since-first-inquiry>1090</number-of-days-since-first-inquiry>
            <number-of-days-since-last-inquiry>25</number-of-days-since-last-inquiry>
            <no-previous-inquiry>false</no-previous-inquiry>
            <no-non-dda-auto-previous-inquiry nil="true"/>
            <no-non-dda-credit-previous-inquiry nil="true"/>
            <closures>
                <closure>
                    <routing-number>10101010</routing-number>
                    <account-number>11111111111111112222</account-number>
                    <closure-date>2014-10-16</closure-date>
                    <reason-code>Y</reason-code>
                    <institution-name>CHEXSYSTEMS, INC.</institution-name>
                    <institution-state>MN</institution-state>
                    <amount>20000</amount>
                    <paid-date nil="true"/>
                    <consumer-dispute-text></consumer-dispute-text>
                    <days-since-closed>50</days-since-closed>
                    <days-since-paid-closed nil="true"/>
                    <matches-clarity-seen-account>false</matches-clarity-seen-account>
                </closure>
                <closure>
                    <routing-number>10101010</routing-number>
                    <account-number>11111111111111112223</account-number>
                    <closure-date>2014-10-13</closure-date>
                    <reason-code>7</reason-code>
                    <institution-name>CHEXSYSTEMS, INC.</institution-name>
                    <institution-state>MN</institution-state>
                    <amount>10000</amount>
                    <paid-date>2014-10-16</paid-date>
                    <consumer-dispute-text></consumer-dispute-text>
                    <days-since-closed>53</days-since-closed>
                    <days-since-paid-closed>50</days-since-paid-closed>
                    <matches-clarity-seen-account>false</matches-clarity-seen-account>
                </closure>
                <closure>
                    <routing-number>10101010</routing-number>
                    <account-number>11111111111111112224</account-number>
                    <closure-date>2014-09-26</closure-date>
                    <reason-code>A</reason-code>
                    <institution-name>CHEXSYSTEMS, INC.</institution-name>
                    <institution-state>MN</institution-state>
                    <amount>20000</amount>
                    <paid-date nil="true"/>
                    <consumer-dispute-text></consumer-dispute-text>
                    <days-since-closed>70</days-since-closed>
                    <days-since-paid-closed nil="true"/>
                    <matches-clarity-seen-account>false</matches-clarity-seen-account>
                </closure>
                <closure>
                    <routing-number>10101010</routing-number>
                    <account-number>11111111111111112225</account-number>
                    <closure-date>2014-09-16</closure-date>
                    <reason-code>Y</reason-code>
                    <institution-name>CHEXSYSTEMS, INC.</institution-name>
                    <institution-state>MN</institution-state>
                    <amount>30000</amount>
                    <paid-date>2014-11-05</paid-date>
                    <consumer-dispute-text></consumer-dispute-text>
                    <days-since-closed>80</days-since-closed>
                    <days-since-paid-closed>30</days-since-paid-closed>
                    <matches-clarity-seen-account>false</matches-clarity-seen-account>
                </closure>
                <closure>
                    <routing-number>10101010</routing-number>
                    <account-number>11111111111111112226</account-number>
                    <closure-date>2014-08-17</closure-date>
                    <reason-code>N</reason-code>
                    <institution-name>CHEXSYSTEMS, INC.</institution-name>
                    <institution-state>MN</institution-state>
                    <amount>10000</amount>
                    <paid-date nil="true"/>
                    <consumer-dispute-text></consumer-dispute-text>
                    <days-since-closed>110</days-since-closed>
                    <days-since-paid-closed nil="true"/>
                    <matches-clarity-seen-account>false</matches-clarity-seen-account>
                </closure>
            </closures>
            <returned-checks>
                <returned-check>
                    <routing-number>10101010</routing-number>
                    <account-number>11111111111111112222</account-number>
                    <drivers-license-number>xxxxxxxxx0088</drivers-license-number>
                    <drivers-license-state>FL</drivers-license-state>
                    <merchant-name>SAM'S SERVICE STATION</merchant-name>
                    <check-date>2014-10-16</check-date>
                    <check-number>00001002</check-number>
                    <amount>10000</amount>
                    <paid-date nil="true"/>
                    <days-since-check-returned>50</days-since-check-returned>
                    <days-since-check-paid nil="true"/>
                    <drivers-license-id>567ac37251ce41ee86895618</drivers-license-id>
                    <matches-clarity-seen-account>false</matches-clarity-seen-account>
                </returned-check>
                <returned-check>
                    <routing-number>10101010</routing-number>
                    <account-number>11111111111111112223</account-number>
                    <drivers-license-number>xxxxxxxxx0088</drivers-license-number>
                    <drivers-license-state>FL</drivers-license-state>
                    <merchant-name>SAM'S SERVICE STATION</merchant-name>
                    <check-date>2014-09-16</check-date>
                    <check-number>00001003</check-number>
                    <amount>10000</amount>
                    <paid-date>2014-10-31</paid-date>
                    <days-since-check-returned>80</days-since-check-returned>
                    <days-since-check-paid>35</days-since-check-paid>
                    <drivers-license-id>567ac37251ce41ee86895618</drivers-license-id>
                    <matches-clarity-seen-account>false</matches-clarity-seen-account>
                </returned-check>
                <returned-check>
                    <routing-number>10101010</routing-number>
                    <account-number>111111111111111122244</account-number>
                    <drivers-license-number>xxxxxxxxx0088</drivers-license-number>
                    <drivers-license-state>FL</drivers-license-state>
                    <merchant-name>SAM'S SERVICE STATION</merchant-name>
                    <check-date>2014-08-17</check-date>
                    <check-number>00001004</check-number>
                    <amount>20000</amount>
                    <paid-date nil="true"/>
                    <days-since-check-returned>110</days-since-check-returned>
                    <days-since-check-paid nil="true"/>
                    <drivers-license-id>567ac37251ce41ee86895618</drivers-license-id>
                    <matches-clarity-seen-account>false</matches-clarity-seen-account>
                </returned-check>
                <returned-check>
                    <routing-number>10101010</routing-number>
                    <account-number>11111111111111112225</account-number>
                    <drivers-license-number>xxxxxxxxx0088</drivers-license-number>
                    <drivers-license-state>FL</drivers-license-state>
                    <merchant-name>SAM'S SERVICE STATION</merchant-name>
                    <check-date>2014-05-24</check-date>
                    <check-number>00001005</check-number>
                    <amount>30000</amount>
                    <paid-date>2014-10-21</paid-date>
                    <days-since-check-returned>195</days-since-check-returned>
                    <days-since-check-paid>45</days-since-check-paid>
                    <drivers-license-id>567ac37251ce41ee86895618</drivers-license-id>
                    <matches-clarity-seen-account>false</matches-clarity-seen-account>
                </returned-check>
                <returned-check>
                    <routing-number>10101010</routing-number>
                    <account-number>11111111111111112226</account-number>
                    <drivers-license-number>xxxxxxxxx0088</drivers-license-number>
                    <drivers-license-state>FL</drivers-license-state>
                    <merchant-name>SAM'S SERVICE STATION</merchant-name>
                    <check-date>2014-04-04</check-date>
                    <check-number>00001006</check-number>
                    <amount>40000</amount>
                    <paid-date nil="true"/>
                    <days-since-check-returned>245</days-since-check-returned>
                    <days-since-check-paid nil="true"/>
                    <drivers-license-id>567ac37251ce41ee86895618</drivers-license-id>
                    <matches-clarity-seen-account>false</matches-clarity-seen-account>
                </returned-check>
            </returned-checks>
            <amount-fraud-closures>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <four-years-ago nil="true"/>
                <five-years-ago nil="true"/>
            </amount-fraud-closures>
            <number-fraud-closure>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <four-years-ago nil="true"/>
                <five-years-ago nil="true"/>
            </number-fraud-closure>
            <amount-closures>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <four-years-ago nil="true"/>
                <five-years-ago nil="true"/>
            </amount-closures>
            <amount-closures-paid>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <four-years-ago nil="true"/>
                <five-years-ago nil="true"/>
            </amount-closures-paid>
            <amount-closures-unpaid>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <four-years-ago nil="true"/>
                <five-years-ago nil="true"/>
            </amount-closures-unpaid>
            <number-closures>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <four-years-ago nil="true"/>
                <five-years-ago nil="true"/>
            </number-closures>
            <number-closures-paid>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <four-years-ago nil="true"/>
                <five-years-ago nil="true"/>
            </number-closures-paid>
            <number-closures-unpaid>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <four-years-ago nil="true"/>
                <five-years-ago nil="true"/>
            </number-closures-unpaid>
            <number-of-checks-ordered>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </number-of-checks-ordered>
            <number-of-check-orders>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </number-of-check-orders>
            <number-of-non-dda-inquiries>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <number-since-first-inquiry nil="true"/>
                <number-since-last-inquiry nil="true"/>
            </number-of-non-dda-inquiries>
            <number-of-payday-inquiries>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <number-since-first-inquiry nil="true"/>
                <number-since-last-inquiry nil="true"/>
            </number-of-payday-inquiries>
            <number-of-inquiries>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <number-since-first-inquiry nil="true"/>
                <number-since-last-inquiry nil="true"/>
            </number-of-inquiries>
            <amount-open-items>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </amount-open-items>
            <amount-paid-items>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </amount-paid-items>
            <number-open-items>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </number-open-items>
            <number-paid-items>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </number-paid-items>
        </fis-chex-advisor>
        <accounts>
            <account>
                <account-index>1</account-index>
                <primary>true</primary>
                <account-risk-level>high</account-risk-level>
                <high-risk-factors>2</high-risk-factors>
                <days-since-last-seen-by-clarity>40</days-since-last-seen-by-clarity>
                <days-since-first-seen-by-clarity>40</days-since-first-seen-by-clarity>
                <number-of-ssns>2</number-of-ssns>
                <default-history nil="true"/>
                <validated-through-trades nil="true"/>
                <inquiries-30-days-ago>0</inquiries-30-days-ago>
                <inquiries-31-365-days-ago>1</inquiries-31-365-days-ago>
                <inquiries-ratio>0.0</inquiries-ratio>
                <inquiries-app-state-30-days-ago>0</inquiries-app-state-30-days-ago>
                <inquiries-app-state-31-365-days-ago>1</inquiries-app-state-31-365-days-ago>
                <inquiries-app-state-ratio>0.0</inquiries-app-state-ratio>
                <default-rate-60-days-ago nil="true"/>
                <default-rate-61-365-days-ago nil="true"/>
                <default-rate-ratio nil="true"/>
                <bank-risk-level nil="true"/>
                <account-age-code>2</account-age-code>
                <days-since-validated-trade nil="true"/>
                <days-since-default-history nil="true"/>
                <stability>
                    <one-hour-ago>1</one-hour-ago>
                    <twentyfour-hours-ago>1</twentyfour-hours-ago>
                    <seven-days-ago>1</seven-days-ago>
                    <fifteen-days-ago>1</fifteen-days-ago>
                    <thirty-days-ago>1</thirty-days-ago>
                    <ninety-days-ago>2</ninety-days-ago>
                    <one-hundred-eighty-days-ago>2</one-hundred-eighty-days-ago>
                    <one-year-ago>2</one-year-ago>
                </stability>
                <reason-codes>A01(#)|A07|A49|A50|A51|A52</reason-codes>
                <bank-name></bank-name>
                <account-number></account-number>
                <routing-number nil="true"/>
            </account>
            <account>
                <account-index>2</account-index>
                <primary>false</primary>
                <account-risk-level>high</account-risk-level>
                <high-risk-factors>1</high-risk-factors>
                <days-since-last-seen-by-clarity>10</days-since-last-seen-by-clarity>
                <days-since-first-seen-by-clarity>50</days-since-first-seen-by-clarity>
                <number-of-ssns>2</number-of-ssns>
                <default-history nil="true"/>
                <validated-through-trades nil="true"/>
                <inquiries-30-days-ago>7</inquiries-30-days-ago>
                <inquiries-31-365-days-ago>18</inquiries-31-365-days-ago>
                <inquiries-ratio>0.28</inquiries-ratio>
                <inquiries-app-state-30-days-ago>7</inquiries-app-state-30-days-ago>
                <inquiries-app-state-31-365-days-ago>18</inquiries-app-state-31-365-days-ago>
                <inquiries-app-state-ratio>0.28</inquiries-app-state-ratio>
                <default-rate-60-days-ago nil="true"/>
                <default-rate-61-365-days-ago nil="true"/>
                <default-rate-ratio nil="true"/>
                <bank-risk-level nil="true"/>
                <account-age-code>2</account-age-code>
                <days-since-validated-trade nil="true"/>
                <days-since-default-history nil="true"/>
                <stability>
                    <one-hour-ago>1</one-hour-ago>
                    <twentyfour-hours-ago>1</twentyfour-hours-ago>
                    <seven-days-ago>1</seven-days-ago>
                    <fifteen-days-ago>4</fifteen-days-ago>
                    <thirty-days-ago>4</thirty-days-ago>
                    <ninety-days-ago>5</ninety-days-ago>
                    <one-hundred-eighty-days-ago>5</one-hundred-eighty-days-ago>
                    <one-year-ago>5</one-year-ago>
                </stability>
                <reason-codes>B01(#)|B07|B11</reason-codes>
                <bank-name></bank-name>
                <account-number></account-number>
                <routing-number nil="true"/>
            </account>
            <account>
                <account-index>3</account-index>
                <primary>false</primary>
                <account-risk-level>unk</account-risk-level>
                <high-risk-factors>0</high-risk-factors>
                <days-since-last-seen-by-clarity>20</days-since-last-seen-by-clarity>
                <days-since-first-seen-by-clarity>80</days-since-first-seen-by-clarity>
                <number-of-ssns>1</number-of-ssns>
                <default-history>true</default-history>
                <validated-through-trades>true</validated-through-trades>
                <inquiries-30-days-ago>1</inquiries-30-days-ago>
                <inquiries-31-365-days-ago>1</inquiries-31-365-days-ago>
                <inquiries-ratio>0.5</inquiries-ratio>
                <inquiries-app-state-30-days-ago>1</inquiries-app-state-30-days-ago>
                <inquiries-app-state-31-365-days-ago>1</inquiries-app-state-31-365-days-ago>
                <inquiries-app-state-ratio>0.5</inquiries-app-state-ratio>
                <default-rate-60-days-ago>50.0</default-rate-60-days-ago>
                <default-rate-61-365-days-ago>50.0</default-rate-61-365-days-ago>
                <default-rate-ratio>1.0</default-rate-ratio>
                <bank-risk-level>true</bank-risk-level>
                <account-age-code>3</account-age-code>
                <days-since-validated-trade>27</days-since-validated-trade>
                <days-since-default-history>1</days-since-default-history>
                <stability>
                    <one-hour-ago>1</one-hour-ago>
                    <twentyfour-hours-ago>1</twentyfour-hours-ago>
                    <seven-days-ago>1</seven-days-ago>
                    <fifteen-days-ago>1</fifteen-days-ago>
                    <thirty-days-ago>2</thirty-days-ago>
                    <ninety-days-ago>3</ninety-days-ago>
                    <one-hundred-eighty-days-ago>3</one-hundred-eighty-days-ago>
                    <one-year-ago>3</one-year-ago>
                </stability>
                <reason-codes>B01(#)|B08</reason-codes>
                <bank-name></bank-name>
                <account-number></account-number>
                <routing-number nil="true"/>
            </account>
            <account>
                <account-index>4</account-index>
                <primary>false</primary>
                <account-risk-level>low</account-risk-level>
                <high-risk-factors>0</high-risk-factors>
                <days-since-last-seen-by-clarity>28</days-since-last-seen-by-clarity>
                <days-since-first-seen-by-clarity>400</days-since-first-seen-by-clarity>
                <number-of-ssns>1</number-of-ssns>
                <default-history>false</default-history>
                <validated-through-trades>true</validated-through-trades>
                <inquiries-30-days-ago>1</inquiries-30-days-ago>
                <inquiries-31-365-days-ago>0</inquiries-31-365-days-ago>
                <inquiries-ratio>1.0</inquiries-ratio>
                <inquiries-app-state-30-days-ago>1</inquiries-app-state-30-days-ago>
                <inquiries-app-state-31-365-days-ago>0</inquiries-app-state-31-365-days-ago>
                <inquiries-app-state-ratio>1.0</inquiries-app-state-ratio>
                <default-rate-60-days-ago nil="true"/>
                <default-rate-61-365-days-ago nil="true"/>
                <default-rate-ratio nil="true"/>
                <bank-risk-level nil="true"/>
                <account-age-code>6</account-age-code>
                <days-since-validated-trade>360</days-since-validated-trade>
                <days-since-default-history nil="true"/>
                <stability>
                    <one-hour-ago>1</one-hour-ago>
                    <twentyfour-hours-ago>1</twentyfour-hours-ago>
                    <seven-days-ago>1</seven-days-ago>
                    <fifteen-days-ago>1</fifteen-days-ago>
                    <thirty-days-ago>2</thirty-days-ago>
                    <ninety-days-ago>2</ninety-days-ago>
                    <one-hundred-eighty-days-ago>2</one-hundred-eighty-days-ago>
                    <one-year-ago>2</one-year-ago>
                </stability>
                <reason-codes>B05</reason-codes>
                <bank-name></bank-name>
                <account-number></account-number>
                <routing-number nil="true"/>
            </account>
            <account>
                <account-index>5</account-index>
                <primary>false</primary>
                <account-risk-level>low</account-risk-level>
                <high-risk-factors>0</high-risk-factors>
                <days-since-last-seen-by-clarity>2</days-since-last-seen-by-clarity>
                <days-since-first-seen-by-clarity>477</days-since-first-seen-by-clarity>
                <number-of-ssns>21</number-of-ssns>
                <default-history>true</default-history>
                <validated-through-trades>true</validated-through-trades>
                <inquiries-30-days-ago>7</inquiries-30-days-ago>
                <inquiries-31-365-days-ago>18</inquiries-31-365-days-ago>
                <inquiries-ratio>0.28</inquiries-ratio>
                <inquiries-app-state-30-days-ago>7</inquiries-app-state-30-days-ago>
                <inquiries-app-state-31-365-days-ago>18</inquiries-app-state-31-365-days-ago>
                <inquiries-app-state-ratio>0.28</inquiries-app-state-ratio>
                <default-rate-60-days-ago nil="true"/>
                <default-rate-61-365-days-ago nil="true"/>
                <default-rate-ratio nil="true"/>
                <bank-risk-level nil="true"/>
                <account-age-code>6</account-age-code>
                <days-since-validated-trade>180</days-since-validated-trade>
                <days-since-default-history>150</days-since-default-history>
                <stability>
                    <one-hour-ago>1</one-hour-ago>
                    <twentyfour-hours-ago>1</twentyfour-hours-ago>
                    <seven-days-ago>2</seven-days-ago>
                    <fifteen-days-ago>2</fifteen-days-ago>
                    <thirty-days-ago>3</thirty-days-ago>
                    <ninety-days-ago>7</ninety-days-ago>
                    <one-hundred-eighty-days-ago>11</one-hundred-eighty-days-ago>
                    <one-year-ago>18</one-year-ago>
                </stability>
                <reason-codes>B05</reason-codes>
                <bank-name></bank-name>
                <account-number></account-number>
                <routing-number nil="true"/>
            </account>
        </accounts>
        <reason-codes>A01(#)|A07|A49|A50|A51|A52|B01(#)|B05|B07|B08|B11</reason-codes>
        <cbb-reason-codes>BB109(*)|BB108(*)|BB103|BB110(*)</cbb-reason-codes>
        <cbb-reason-codes2 nil="true"/>
        <inquiry-cluster-account-stability>
            <one-hour-ago>1.0</one-hour-ago>
            <twentyfour-hours-ago>1.0</twentyfour-hours-ago>
            <seven-days-ago>1.2</seven-days-ago>
            <fifteen-days-ago>1.8</fifteen-days-ago>
            <thirty-days-ago>2.4</thirty-days-ago>
            <ninety-days-ago>3.8</ninety-days-ago>
            <one-hundred-eighty-days-ago>4.6</one-hundred-eighty-days-ago>
            <one-year-ago>6.0</one-year-ago>
        </inquiry-cluster-account-stability>
        <account-stability>
            <one-hour-ago>5</one-hour-ago>
            <twentyfour-hours-ago>5</twentyfour-hours-ago>
            <seven-days-ago>5</seven-days-ago>
            <fifteen-days-ago>5</fifteen-days-ago>
            <thirty-days-ago>5</thirty-days-ago>
            <ninety-days-ago>5</ninety-days-ago>
            <one-hundred-eighty-days-ago>5</one-hundred-eighty-days-ago>
            <one-year-ago>5</one-year-ago>
        </account-stability>
        <inquiry-cluster-stability>
            <one-hour-ago>5</one-hour-ago>
            <twentyfour-hours-ago>5</twentyfour-hours-ago>
            <seven-days-ago>6</seven-days-ago>
            <fifteen-days-ago>9</fifteen-days-ago>
            <thirty-days-ago>12</thirty-days-ago>
            <ninety-days-ago>19</ninety-days-ago>
            <one-hundred-eighty-days-ago>23</one-hundred-eighty-days-ago>
            <one-year-ago>30</one-year-ago>
        </inquiry-cluster-stability>
        <check-cashing>
            <call-time-msec>1</call-time-msec>
            <micr-ssn-24months>24</micr-ssn-24months>
            <days-since-last-check-cashed>10</days-since-last-check-cashed>
            <count-checks-cashed>3</count-checks-cashed>
            <days-since-last-check-attempted>10</days-since-last-check-attempted>
            <micr-ssn-24months-attempted>14</micr-ssn-24months-attempted>
            <count-of-checks-cashed>
                <name>count_of_checks_cashed</name>
                <twentyfour-hours-ago>1</twentyfour-hours-ago>
                <seven-days-ago>7</seven-days-ago>
                <thirty-days-ago>30</thirty-days-ago>
                <ninety-days-ago>90</ninety-days-ago>
                <one-hundred-eighty-days-ago>180</one-hundred-eighty-days-ago>
                <one-year-ago>100</one-year-ago>
                <two-years-ago>200</two-years-ago>
            </count-of-checks-cashed>
            <count-of-checks-attempted>
                <name>count_of_checks_attempted</name>
                <twentyfour-hours-ago>10</twentyfour-hours-ago>
                <seven-days-ago>70</seven-days-ago>
                <thirty-days-ago>300</thirty-days-ago>
                <ninety-days-ago>900</ninety-days-ago>
                <one-hundred-eighty-days-ago>1800</one-hundred-eighty-days-ago>
                <one-year-ago>1000</one-year-ago>
                <two-years-ago>2000</two-years-ago>
            </count-of-checks-attempted>
            <amount-of-checks-cashed>
                <name>amount_of_checks_cashed</name>
                <twentyfour-hours-ago>112</twentyfour-hours-ago>
                <seven-days-ago>734</seven-days-ago>
                <thirty-days-ago>3056</thirty-days-ago>
                <ninety-days-ago>9078</ninety-days-ago>
                <one-hundred-eighty-days-ago>18090</one-hundred-eighty-days-ago>
                <one-year-ago>50000</one-year-ago>
                <two-years-ago>103275</two-years-ago>
            </amount-of-checks-cashed>
            <amount-of-checks-attempted>
                <name>amount_of_checks_attempted</name>
                <twentyfour-hours-ago>1112</twentyfour-hours-ago>
                <seven-days-ago>1734</seven-days-ago>
                <thirty-days-ago>13056</thirty-days-ago>
                <ninety-days-ago>19078</ninety-days-ago>
                <one-hundred-eighty-days-ago>118090</one-hundred-eighty-days-ago>
                <one-year-ago>150000</one-year-ago>
                <two-years-ago>1103275</two-years-ago>
            </amount-of-checks-attempted>
            <average-amount-of-checks-cashed>
                <name>average_amount_of_checks_cashed</name>
                <twentyfour-hours-ago>10009</twentyfour-hours-ago>
                <seven-days-ago>70087</seven-days-ago>
                <thirty-days-ago>300065</thirty-days-ago>
                <ninety-days-ago>900043</ninety-days-ago>
                <one-hundred-eighty-days-ago>1800021</one-hundred-eighty-days-ago>
                <one-year-ago>50000</one-year-ago>
                <two-years-ago>34425</two-years-ago>
            </average-amount-of-checks-cashed>
            <average-amount-of-checks-attempted>
                <name>average_amount_of_checks_attempted</name>
                <twentyfour-hours-ago>110009</twentyfour-hours-ago>
                <seven-days-ago>170087</seven-days-ago>
                <thirty-days-ago>1300065</thirty-days-ago>
                <ninety-days-ago>1900043</ninety-days-ago>
                <one-hundred-eighty-days-ago>11800021</one-hundred-eighty-days-ago>
                <one-year-ago>150000</one-year-ago>
                <two-years-ago>134425</two-years-ago>
            </average-amount-of-checks-attempted>
        </check-cashing>
    </clear-bank-behavior>
    <clear-credit-risk>
        <action>Approve</action>
        <deny-codes>111</deny-codes>
        <deny-descriptions>(111) You did so well</deny-descriptions>
        <exception-descriptions nil="true"/>
        <clarity-seen nil="true"/>
        <error-code nil="true"/>
        <error-description nil="true"/>
        <hit nil="true"/>
        <messages nil="true"/>
        <too-many-inquiries nil="true"/>
        <too-many-tradelines nil="true"/>
        <full-name>FENCE, JOHN </full-name>
        <product-date>2019-05-23T22:32:00Z</product-date>
        <filter-codes>111</filter-codes>
        <filter-descriptions>(111) You did so well</filter-descriptions>
        <stabilities>
            <stability>
                <name>C1.inq</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.nto</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.ato</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.dd</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.nlp</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.alp</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.dlp</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.ncl</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.acl</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.dcl</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.nco</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.aco</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.inq</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.nto</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.ato</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.dd</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.nlp</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.alp</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.dlp</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.ncl</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.acl</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.dcl</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.nco</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.aco</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>CA.inq</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.nto</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.ato</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.dd</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.nlp</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.alp</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.dlp</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.ncl</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.acl</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.dcl</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.nco</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.aco</name>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
        </stabilities>
    </clear-credit-risk>
    <clear-advanced-attributes>
        <action>Approve</action>
        <deny-codes nil="true"/>
        <deny-descriptions nil="true"/>
        <exception-descriptions nil="true"/>
        <clarity-seen nil="true"/>
        <error-code nil="true"/>
        <error-description nil="true"/>
        <hit nil="true"/>
        <messages nil="true"/>
        <too-many-inquiries nil="true"/>
        <too-many-tradelines nil="true"/>
        <full-name>FENCE, JOHN </full-name>
        <product-date>2019-05-23T22:32:00Z</product-date>
        <filter-codes nil="true"/>
        <filter-descriptions nil="true"/>
        <stabilities>
            <stability>
                <name>C1.inq</name>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.nto</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.ato</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.dd</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.nlp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.alp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.dlp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.ncl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.acl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.dcl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.nco</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C1.aco</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.inq</name>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.nto</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.ato</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.dd</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.nlp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.alp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.dlp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.ncl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.acl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.dcl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.nco</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>C3.aco</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>48.inq</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </stability>
            <stability>
                <name>CA.inq</name>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </stability>
            <stability>
                <name>CA.clu</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C2.inq</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C2.nto</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C2.ato</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C2.dd</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C2.nlp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C2.alp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C2.dlp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C2.ncl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C2.acl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C2.dcl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C2.nco</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C2.aco</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C6.inq</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C6.nto</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C6.ato</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C6.dd</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C6.nlp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C6.alp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C6.dlp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C6.ncl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C6.acl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C6.dcl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C6.nco</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C6.aco</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
                <seven-years-ago nil="true"/>
            </stability>
            <stability>
                <name>C5.inq</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </stability>
            <stability>
                <name>M2.inq</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
                <fifteen-days-ago nil="true"/>
                <thirty-days-ago nil="true"/>
                <sixty-days-ago nil="true"/>
                <ninety-days-ago nil="true"/>
                <one-hundred-eighty-days-ago nil="true"/>
                <one-year-ago nil="true"/>
                <two-years-ago nil="true"/>
                <three-years-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.nto</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.ato</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.dd</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.nlp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.alp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.dlp</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.ncl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.acl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.dcl</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.nco</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
            <stability>
                <name>ALL.aco</name>
                <twentyfour-hours-ago nil="true"/>
                <seven-days-ago nil="true"/>
            </stability>
        </stabilities>
    </clear-advanced-attributes>
    <clear-fraud>
        <action>Approve</action>
        <deny-codes nil="true"/>
        <deny-descriptions nil="true"/>
        <exception-descriptions nil="true"/>
        <clear-fraud-score>618</clear-fraud-score>
        <non-scorable-reason-code>xx</non-scorable-reason-code>
        <non-scorable-reason-description nil="true"/>
        <clear-fraud-reason-codes>C205|C214|C213|C212</clear-fraud-reason-codes>
        <clear-fraud-reason-code-description>(C205) High risk due to lack of auto loan/lease trades |(C214) Lack of open installment trades with a balance or reported in the last 6 months |(C213) Lack of or too few bankcards trades |(C212) Too many credit inquiries in the last 12 months </clear-fraud-reason-code-description>
        <crosstab-points-total>91</crosstab-points-total>
        <crosstab-multiple>1.62</crosstab-multiple>
        <error-code nil="true"/>
        <bankruptcy-codes>B07|B13</bankruptcy-codes>
        <bankruptcy-description>(B07) Chapter 7 Bankruptcy Reported|(B13) Chapter 13 Bankruptcy Reported</bankruptcy-description>
        <fraud-signature-match>false</fraud-signature-match>
        <fraud-signature-identifier nil="true"/>
        <fraud-signature-name nil="true"/>
        <error-description nil="true"/>
        <message nil="true"/>
        <score-version nil="true"/>
        <full-name>FENCE, JOHN </full-name>
        <product-date>2019-05-23T22:32:00Z</product-date>
        <filter-codes nil="true"/>
        <filter-descriptions nil="true"/>
        <clear-fraud-identity-verification>
            <overall-match-reason-code>6</overall-match-reason-code>
            <overall-match-result>partial</overall-match-result>
            <ssn-name-match>match</ssn-name-match>
            <name-address-match>partial</name-address-match>
            <ssn-dob-match>match</ssn-dob-match>
            <name-address-reason-code nil="true"/>
            <ssn-name-reason-code nil="true"/>
            <ssn-dob-reason-code nil="true"/>
            <name-address-reason-code-description nil="true"/>
            <ssn-name-reason-code-description nil="true"/>
            <ssn-dob-reason-code-description nil="true"/>
            <phone-match-result nil="true"/>
            <phone-match-type nil="true"/>
            <phone-match-type-description nil="true"/>
            <phone-type nil="true"/>
        </clear-fraud-identity-verification>
        <clear-fraud-alternate-identity>
            <found-with-bank-account>false</found-with-bank-account>
            <fraud-risk nil="true"/>
            <identity-type-code nil="true"/>
            <identity-type-description nil="true"/>
            <credit-risk nil="true"/>
        </clear-fraud-alternate-identity>
        <clear-fraud-indicator>
            <total-number-of-fraud-indicators>4</total-number-of-fraud-indicators>
            <input-ssn-issue-date-cannot-be-verified>false</input-ssn-issue-date-cannot-be-verified>
            <input-ssn-recorded-as-deceased>false</input-ssn-recorded-as-deceased>
            <input-ssn-invalid>false</input-ssn-invalid>
            <best-on-file-ssn-issue-date-cannot-be-verified>false</best-on-file-ssn-issue-date-cannot-be-verified>
            <best-on-file-ssn-recorded-as-deceased>false</best-on-file-ssn-recorded-as-deceased>
            <high-probability-ssn-belongs-to-another>false</high-probability-ssn-belongs-to-another>
            <ssn-reported-more-frequently-for-another>false</ssn-reported-more-frequently-for-another>
            <inquiry-age-younger-than-ssn-issue-date>false</inquiry-age-younger-than-ssn-issue-date>
            <credit-established-before-age-18>false</credit-established-before-age-18>
            <credit-established-prior-to-ssn-issue-date>false</credit-established-prior-to-ssn-issue-date>
            <more-than-3-inquiries-in-the-last-30-days>false</more-than-3-inquiries-in-the-last-30-days>
            <drivers-license-inconsistent-with-on-file>false</drivers-license-inconsistent-with-on-file>
            <drivers-license-format-invalid>false</drivers-license-format-invalid>
            <max-number-of-ssns-with-any-bank-account>2</max-number-of-ssns-with-any-bank-account>
            <inquiry-on-file-current-address-conflict>true</inquiry-on-file-current-address-conflict>
            <inquiry-address-first-reported-lt-90-days>false</inquiry-address-first-reported-lt-90-days>
            <inquiry-current-address-not-on-file>true</inquiry-current-address-not-on-file>
            <inquiry-address-high-risk>false</inquiry-address-high-risk>
            <inquiry-address-non-residential>false</inquiry-address-non-residential>
            <inquiry-address-cautious>false</inquiry-address-cautious>
            <on-file-address-high-risk>false</on-file-address-high-risk>
            <on-file-address-non-residential>false</on-file-address-non-residential>
            <on-file-address-cautious>false</on-file-address-cautious>
            <current-address-reported-by-new-trade-only>false</current-address-reported-by-new-trade-only>
            <current-address-reported-by-trade-open-lt-90-days>false</current-address-reported-by-trade-open-lt-90-days>
            <telephone-number-inconsistent-with-address>true</telephone-number-inconsistent-with-address>
            <telephone-number-inconsistent-with-state>false</telephone-number-inconsistent-with-state>
            <work-phone-previously-listed-as-cell-phone>true</work-phone-previously-listed-as-cell-phone>
            <work-phone-previously-listed-as-home-phone>false</work-phone-previously-listed-as-home-phone>
        </clear-fraud-indicator>
        <clear-fraud-inquiry>
            <one-minute-ago>3</one-minute-ago>
            <ten-minutes-ago>6</ten-minutes-ago>
            <one-hour-ago>6</one-hour-ago>
            <twentyfour-hours-ago>6</twentyfour-hours-ago>
            <seven-days-ago>6</seven-days-ago>
            <fifteen-days-ago>6</fifteen-days-ago>
            <thirty-days-ago>6</thirty-days-ago>
            <ninety-days-ago>7</ninety-days-ago>
            <threesixtyfive-days-ago>11</threesixtyfive-days-ago>
        </clear-fraud-inquiry>
        <clear-fraud-points-total>
            <one-minute-ago>9</one-minute-ago>
            <ten-minutes-ago>10</ten-minutes-ago>
            <one-hour-ago>10</one-hour-ago>
            <twentyfour-hours-ago>10</twentyfour-hours-ago>
            <seven-days-ago>10</seven-days-ago>
            <fifteen-days-ago>10</fifteen-days-ago>
            <thirty-days-ago>10</thirty-days-ago>
            <ninety-days-ago>13</ninety-days-ago>
            <threesixtyfive-days-ago>25</threesixtyfive-days-ago>
        </clear-fraud-points-total>
        <clear-fraud-multiple>
            <one-minute-ago>1.0</one-minute-ago>
            <ten-minutes-ago>1.11</ten-minutes-ago>
            <one-hour-ago>1.11</one-hour-ago>
            <twentyfour-hours-ago>1.11</twentyfour-hours-ago>
            <seven-days-ago>1.11</seven-days-ago>
            <fifteen-days-ago>1.11</fifteen-days-ago>
            <thirty-days-ago>1.11</thirty-days-ago>
            <ninety-days-ago>1.44</ninety-days-ago>
            <threesixtyfive-days-ago>2.77</threesixtyfive-days-ago>
        </clear-fraud-multiple>
        <clear-fraud-stabilities>
            <clear-fraud-stability>
                <name>drivers_license</name>
                <one-minute-ago>1</one-minute-ago>
                <ten-minutes-ago>1</ten-minutes-ago>
                <one-hour-ago>1</one-hour-ago>
                <twentyfour-hours-ago>1</twentyfour-hours-ago>
                <seven-days-ago>1</seven-days-ago>
                <fifteen-days-ago>1</fifteen-days-ago>
                <thirty-days-ago>1</thirty-days-ago>
                <ninety-days-ago>2</ninety-days-ago>
                <threesixtyfive-days-ago>3</threesixtyfive-days-ago>
            </clear-fraud-stability>
            <clear-fraud-stability>
                <name>bank_account</name>
                <one-minute-ago>1</one-minute-ago>
                <ten-minutes-ago>1</ten-minutes-ago>
                <one-hour-ago>1</one-hour-ago>
                <twentyfour-hours-ago>1</twentyfour-hours-ago>
                <seven-days-ago>1</seven-days-ago>
                <fifteen-days-ago>1</fifteen-days-ago>
                <thirty-days-ago>1</thirty-days-ago>
                <ninety-days-ago>2</ninety-days-ago>
                <threesixtyfive-days-ago>3</threesixtyfive-days-ago>
            </clear-fraud-stability>
            <clear-fraud-stability>
                <name>home_address</name>
                <one-minute-ago>1</one-minute-ago>
                <ten-minutes-ago>1</ten-minutes-ago>
                <one-hour-ago>1</one-hour-ago>
                <twentyfour-hours-ago>1</twentyfour-hours-ago>
                <seven-days-ago>1</seven-days-ago>
                <fifteen-days-ago>1</fifteen-days-ago>
                <thirty-days-ago>1</thirty-days-ago>
                <ninety-days-ago>1</ninety-days-ago>
                <threesixtyfive-days-ago>2</threesixtyfive-days-ago>
            </clear-fraud-stability>
            <clear-fraud-stability>
                <name>zip_code</name>
                <one-minute-ago>1</one-minute-ago>
                <ten-minutes-ago>1</ten-minutes-ago>
                <one-hour-ago>1</one-hour-ago>
                <twentyfour-hours-ago>1</twentyfour-hours-ago>
                <seven-days-ago>1</seven-days-ago>
                <fifteen-days-ago>1</fifteen-days-ago>
                <thirty-days-ago>1</thirty-days-ago>
                <ninety-days-ago>1</ninety-days-ago>
                <threesixtyfive-days-ago>2</threesixtyfive-days-ago>
            </clear-fraud-stability>
            <clear-fraud-stability>
                <name>home_phone</name>
                <one-minute-ago>1</one-minute-ago>
                <ten-minutes-ago>1</ten-minutes-ago>
                <one-hour-ago>1</one-hour-ago>
                <twentyfour-hours-ago>1</twentyfour-hours-ago>
                <seven-days-ago>1</seven-days-ago>
                <fifteen-days-ago>1</fifteen-days-ago>
                <thirty-days-ago>1</thirty-days-ago>
                <ninety-days-ago>1</ninety-days-ago>
                <threesixtyfive-days-ago>2</threesixtyfive-days-ago>
            </clear-fraud-stability>
            <clear-fraud-stability>
                <name>cell_phone</name>
                <one-minute-ago>1</one-minute-ago>
                <ten-minutes-ago>2</ten-minutes-ago>
                <one-hour-ago>2</one-hour-ago>
                <twentyfour-hours-ago>2</twentyfour-hours-ago>
                <seven-days-ago>2</seven-days-ago>
                <fifteen-days-ago>2</fifteen-days-ago>
                <thirty-days-ago>2</thirty-days-ago>
                <ninety-days-ago>2</ninety-days-ago>
                <threesixtyfive-days-ago>3</threesixtyfive-days-ago>
            </clear-fraud-stability>
            <clear-fraud-stability>
                <name>work_phone</name>
                <one-minute-ago>1</one-minute-ago>
                <ten-minutes-ago>1</ten-minutes-ago>
                <one-hour-ago>1</one-hour-ago>
                <twentyfour-hours-ago>1</twentyfour-hours-ago>
                <seven-days-ago>1</seven-days-ago>
                <fifteen-days-ago>1</fifteen-days-ago>
                <thirty-days-ago>1</thirty-days-ago>
                <ninety-days-ago>1</ninety-days-ago>
                <threesixtyfive-days-ago>3</threesixtyfive-days-ago>
            </clear-fraud-stability>
            <clear-fraud-stability>
                <name>monthly_income</name>
                <one-minute-ago>1</one-minute-ago>
                <ten-minutes-ago>1</ten-minutes-ago>
                <one-hour-ago>1</one-hour-ago>
                <twentyfour-hours-ago>1</twentyfour-hours-ago>
                <seven-days-ago>1</seven-days-ago>
                <fifteen-days-ago>1</fifteen-days-ago>
                <thirty-days-ago>1</thirty-days-ago>
                <ninety-days-ago>2</ninety-days-ago>
                <threesixtyfive-days-ago>5</threesixtyfive-days-ago>
            </clear-fraud-stability>
            <clear-fraud-stability>
                <name>email_address</name>
                <one-minute-ago>1</one-minute-ago>
                <ten-minutes-ago>1</ten-minutes-ago>
                <one-hour-ago>1</one-hour-ago>
                <twentyfour-hours-ago>1</twentyfour-hours-ago>
                <seven-days-ago>1</seven-days-ago>
                <fifteen-days-ago>1</fifteen-days-ago>
                <thirty-days-ago>1</thirty-days-ago>
                <ninety-days-ago>1</ninety-days-ago>
                <threesixtyfive-days-ago>2</threesixtyfive-days-ago>
            </clear-fraud-stability>
        </clear-fraud-stabilities>
        <clear-fraud-crosstabs>
            <clear-fraud-crosstab>
                <name>ssn</name>
                <ssn>1</ssn>
                <drivers-license>3</drivers-license>
                <bank-account>3</bank-account>
                <home-address>2</home-address>
                <zip-code>2</zip-code>
                <home-phone>2</home-phone>
                <cell-phone>3</cell-phone>
                <email-address>2</email-address>
            </clear-fraud-crosstab>
            <clear-fraud-crosstab>
                <name>drivers_license</name>
                <ssn>1</ssn>
                <drivers-license>1</drivers-license>
                <bank-account>1</bank-account>
                <home-address>1</home-address>
                <zip-code>1</zip-code>
                <home-phone>1</home-phone>
                <cell-phone>2</cell-phone>
                <email-address>1</email-address>
            </clear-fraud-crosstab>
            <clear-fraud-crosstab>
                <name>bank_account</name>
                <ssn>1</ssn>
                <drivers-license>1</drivers-license>
                <bank-account>1</bank-account>
                <home-address>1</home-address>
                <zip-code>1</zip-code>
                <home-phone>1</home-phone>
                <cell-phone>2</cell-phone>
                <email-address>1</email-address>
            </clear-fraud-crosstab>
            <clear-fraud-crosstab>
                <name>home_address</name>
                <ssn>1</ssn>
                <drivers-license>2</drivers-license>
                <bank-account>2</bank-account>
                <home-address>1</home-address>
                <zip-code>1</zip-code>
                <home-phone>1</home-phone>
                <cell-phone>2</cell-phone>
                <email-address>1</email-address>
            </clear-fraud-crosstab>
            <clear-fraud-crosstab>
                <name>home_phone</name>
                <ssn>2</ssn>
                <drivers-license>3</drivers-license>
                <bank-account>4</bank-account>
                <home-address>2</home-address>
                <zip-code>2</zip-code>
                <home-phone>1</home-phone>
                <cell-phone>3</cell-phone>
                <email-address>3</email-address>
            </clear-fraud-crosstab>
            <clear-fraud-crosstab>
                <name>cell_phone</name>
                <ssn>1</ssn>
                <drivers-license>1</drivers-license>
                <bank-account>1</bank-account>
                <home-address>1</home-address>
                <zip-code>1</zip-code>
                <home-phone>1</home-phone>
                <cell-phone>1</cell-phone>
                <email-address>1</email-address>
            </clear-fraud-crosstab>
            <clear-fraud-crosstab>
                <name>email_address</name>
                <ssn>1</ssn>
                <drivers-license>3</drivers-license>
                <bank-account>2</bank-account>
                <home-address>2</home-address>
                <zip-code>2</zip-code>
                <home-phone>2</home-phone>
                <cell-phone>3</cell-phone>
                <email-address>1</email-address>
            </clear-fraud-crosstab>
        </clear-fraud-crosstabs>
        <clear-fraud-ratios>
            <clear-fraud-ratio>
                <name>one_minute_ratio</name>
                <ten-minutes-ago>0.9</ten-minutes-ago>
                <one-hour-ago>0.9</one-hour-ago>
                <twentyfour-hours-ago>0.9</twentyfour-hours-ago>
                <seven-days-ago>0.9</seven-days-ago>
                <fifteen-days-ago>0.9</fifteen-days-ago>
                <thirty-days-ago>0.9</thirty-days-ago>
                <ninety-days-ago>0.69</ninety-days-ago>
                <threesixtyfive-days-ago>0.36</threesixtyfive-days-ago>
            </clear-fraud-ratio>
            <clear-fraud-ratio>
                <name>ten_minutes_ratio</name>
                <one-hour-ago>1.0</one-hour-ago>
                <twentyfour-hours-ago>1.0</twentyfour-hours-ago>
                <seven-days-ago>1.0</seven-days-ago>
                <fifteen-days-ago>1.0</fifteen-days-ago>
                <thirty-days-ago>1.0</thirty-days-ago>
                <ninety-days-ago>0.76</ninety-days-ago>
                <threesixtyfive-days-ago>0.4</threesixtyfive-days-ago>
            </clear-fraud-ratio>
            <clear-fraud-ratio>
                <name>one_hour_ratio</name>
                <twentyfour-hours-ago>1.0</twentyfour-hours-ago>
                <seven-days-ago>1.0</seven-days-ago>
                <fifteen-days-ago>1.0</fifteen-days-ago>
                <thirty-days-ago>1.0</thirty-days-ago>
                <ninety-days-ago>0.76</ninety-days-ago>
                <threesixtyfive-days-ago>0.4</threesixtyfive-days-ago>
            </clear-fraud-ratio>
            <clear-fraud-ratio>
                <name>twentyfour_hours_ratio</name>
                <seven-days-ago>1.0</seven-days-ago>
                <fifteen-days-ago>1.0</fifteen-days-ago>
                <thirty-days-ago>1.0</thirty-days-ago>
                <ninety-days-ago>0.76</ninety-days-ago>
                <threesixtyfive-days-ago>0.4</threesixtyfive-days-ago>
            </clear-fraud-ratio>
            <clear-fraud-ratio>
                <name>seven_days_ratio</name>
                <fifteen-days-ago>1.0</fifteen-days-ago>
                <thirty-days-ago>1.0</thirty-days-ago>
                <ninety-days-ago>0.76</ninety-days-ago>
                <threesixtyfive-days-ago>0.4</threesixtyfive-days-ago>
            </clear-fraud-ratio>
            <clear-fraud-ratio>
                <name>fifteen_days_ratio</name>
                <thirty-days-ago>1.0</thirty-days-ago>
                <ninety-days-ago>0.76</ninety-days-ago>
                <threesixtyfive-days-ago>0.4</threesixtyfive-days-ago>
            </clear-fraud-ratio>
            <clear-fraud-ratio>
                <name>thirty_days_ratio</name>
                <ninety-days-ago>0.76</ninety-days-ago>
                <threesixtyfive-days-ago>0.4</threesixtyfive-days-ago>
            </clear-fraud-ratio>
            <clear-fraud-ratio>
                <name>ninety_days_ratio</name>
                <threesixtyfive-days-ago>0.52</threesixtyfive-days-ago>
            </clear-fraud-ratio>
            <clear-fraud-ratio>
                <name>threesixtyfive_days_ratio</name>
            </clear-fraud-ratio>
        </clear-fraud-ratios>
    </clear-fraud>
    <clear-fraud-insight>
        <action>Approve</action>
        <deny-codes nil="true"/>
        <deny-descriptions nil="true"/>
        <exception-descriptions nil="true"/>
        <crosstab-multiple>1.69</crosstab-multiple>
        <crosstab-points-total>95</crosstab-points-total>
        <error-code nil="true"/>
        <error-description nil="true"/>
        <fraud-signature-identifier nil="true"/>
        <fraud-signature-match>false</fraud-signature-match>
        <fraud-signature-match-count>0</fraud-signature-match-count>
        <fraud-signature-name nil="true"/>
        <hit>true</hit>
        <message nil="true"/>
        <non-scorable-reason-code nil="true"/>
        <non-scorable-reason-description nil="true"/>
        <reason-code-description nil="true"/>
        <score nil="true"/>
        <score-version nil="true"/>
        <stability-non-scorable-reason-code nil="true"/>
        <stability-non-scorable-reason-description nil="true"/>
        <stability-reason-code-description>(A104) Number of changes in bank account information|(A102) Number of recent inquiries|(A103) Number of changes in application information|(A105) Number of changes in home phone information</stability-reason-code-description>
        <stability-score>718</stability-score>
        <full-name>FENCE, JOHN </full-name>
        <product-date>2019-05-23T22:32:00Z</product-date>
        <filter-codes nil="true"/>
        <filter-descriptions nil="true"/>
        <identity-verification>
            <dob-match-code>9</dob-match-code>
            <dob-match-description>DOB and MOB exact match, YOB exact match (no +/- 1 year logic accommodation)</dob-match-description>
            <dob-match-result>Match</dob-match-result>
            <drivers-license-match-code>Y</drivers-license-match-code>
            <drivers-license-match-description>Match to full name only</drivers-license-match-description>
            <drivers-license-match-result>Match</drivers-license-match-result>
            <name-address-match-code>S </name-address-match-code>
            <name-address-match-confidence>0</name-address-match-confidence>
            <name-address-match-description>Error, invalid code value of S </name-address-match-description>
            <phone-name-address-match-code>C </phone-name-address-match-code>
            <phone-name-address-match-confidence>0</phone-name-address-match-confidence>
            <phone-name-address-match-description>Error, invalid code value of C </phone-name-address-match-description>
            <ssn-name-address-match-code>Y </ssn-name-address-match-code>
            <ssn-name-address-match-confidence>0</ssn-name-address-match-confidence>
            <ssn-name-address-match-description>Error, invalid code value of Y </ssn-name-address-match-description>
            <phone-type-code nil="true"/>
            <phone-type-description nil="true"/>
        </identity-verification>
        <indicator>
            <bank-type nil="true"/>
            <best-on-file-ssn-issue-date-cannot-be-verified>false</best-on-file-ssn-issue-date-cannot-be-verified>
            <best-on-file-ssn-recorded-as-deceased>false</best-on-file-ssn-recorded-as-deceased>
            <credit-established-before-age-18>false</credit-established-before-age-18>
            <credit-established-prior-to-ssn-issue-date>false</credit-established-prior-to-ssn-issue-date>
            <cross-aba-number-of-ssns-with-bank-account>1</cross-aba-number-of-ssns-with-bank-account>
            <cross-aba-unique-aba-count>1</cross-aba-unique-aba-count>
            <current-address-reported-by-new-trade-only>false</current-address-reported-by-new-trade-only>
            <current-address-reported-by-trade-open-lt-90-days>true</current-address-reported-by-trade-open-lt-90-days>
            <drivers-license-format-invalid>true</drivers-license-format-invalid>
            <high-probability-ssn-belongs-to-another>false</high-probability-ssn-belongs-to-another>
            <in-state-bank-indicator nil="true"/>
            <input-ssn-invalid>false</input-ssn-invalid>
            <input-ssn-issue-date-cannot-be-verified>false</input-ssn-issue-date-cannot-be-verified>
            <input-ssn-recorded-as-deceased>false</input-ssn-recorded-as-deceased>
            <inquiry-address-cautious>false</inquiry-address-cautious>
            <inquiry-address-first-reported-lt-90-days>false</inquiry-address-first-reported-lt-90-days>
            <inquiry-address-high-risk>false</inquiry-address-high-risk>
            <inquiry-address-non-residential>false</inquiry-address-non-residential>
            <inquiry-age-younger-than-ssn-issue-date>false</inquiry-age-younger-than-ssn-issue-date>
            <inquiry-current-address-not-on-file>false</inquiry-current-address-not-on-file>
            <inquiry-on-file-current-address-conflict>true</inquiry-on-file-current-address-conflict>
            <max-number-of-ssns-with-any-bank-account>1</max-number-of-ssns-with-any-bank-account>
            <more-than-3-inquiries-in-the-last-30-days>false</more-than-3-inquiries-in-the-last-30-days>
            <nearest-bank-branch-distance nil="true"/>
            <on-file-address-cautious>false</on-file-address-cautious>
            <on-file-address-high-risk>false</on-file-address-high-risk>
            <on-file-address-non-residential>false</on-file-address-non-residential>
            <ssn-reported-more-frequently-for-another>false</ssn-reported-more-frequently-for-another>
            <telephone-number-inconsistent-with-address>true</telephone-number-inconsistent-with-address>
            <total-number-of-fraud-indicators>4</total-number-of-fraud-indicators>
            <work-phone-previously-listed-as-cell-phone>false</work-phone-previously-listed-as-cell-phone>
            <work-phone-previously-listed-as-home-phone>false</work-phone-previously-listed-as-home-phone>
        </indicator>
        <points-total>
            <one-minute-ago>17</one-minute-ago>
            <ten-minutes-ago>20</ten-minutes-ago>
            <one-hour-ago>22</one-hour-ago>
            <twentyfour-hours-ago>24</twentyfour-hours-ago>
            <seven-days-ago>24</seven-days-ago>
            <fifteen-days-ago>25</fifteen-days-ago>
            <thirty-days-ago>26</thirty-days-ago>
            <ninety-days-ago>30</ninety-days-ago>
            <threesixtyfive-days-ago>36</threesixtyfive-days-ago>
        </points-total>
        <multiple>
            <one-minute-ago>1.89</one-minute-ago>
            <ten-minutes-ago>2.22</ten-minutes-ago>
            <one-hour-ago>2.44</one-hour-ago>
            <twentyfour-hours-ago>2.67</twentyfour-hours-ago>
            <seven-days-ago>2.67</seven-days-ago>
            <fifteen-days-ago>2.78</fifteen-days-ago>
            <thirty-days-ago>2.89</thirty-days-ago>
            <ninety-days-ago>3.33</ninety-days-ago>
            <threesixtyfive-days-ago>4.0</threesixtyfive-days-ago>
        </multiple>
        <stabilities>
            <stability>
                <name>inquiry</name>
                <one-minute-ago>4</one-minute-ago>
                <ten-minutes-ago>6</ten-minutes-ago>
                <one-hour-ago>8</one-hour-ago>
                <twentyfour-hours-ago>13</twentyfour-hours-ago>
                <seven-days-ago>15</seven-days-ago>
                <fifteen-days-ago>17</fifteen-days-ago>
                <thirty-days-ago>19</thirty-days-ago>
                <ninety-days-ago>21</ninety-days-ago>
                <threesixtyfive-days-ago>24</threesixtyfive-days-ago>
            </stability>
            <stability>
                <name>drivers_license</name>
                <one-minute-ago>1</one-minute-ago>
                <ten-minutes-ago>2</ten-minutes-ago>
                <one-hour-ago>3</one-hour-ago>
                <twentyfour-hours-ago>3</twentyfour-hours-ago>
                <seven-days-ago>3</seven-days-ago>
                <fifteen-days-ago>3</fifteen-days-ago>
                <thirty-days-ago>3</thirty-days-ago>
                <ninety-days-ago>4</ninety-days-ago>
                <threesixtyfive-days-ago>4</threesixtyfive-days-ago>
            </stability>
            <stability>
                <name>bank_account</name>
                <one-minute-ago>2</one-minute-ago>
                <ten-minutes-ago>3</ten-minutes-ago>
                <one-hour-ago>3</one-hour-ago>
                <twentyfour-hours-ago>3</twentyfour-hours-ago>
                <seven-days-ago>3</seven-days-ago>
                <fifteen-days-ago>4</fifteen-days-ago>
                <thirty-days-ago>4</thirty-days-ago>
                <ninety-days-ago>4</ninety-days-ago>
                <threesixtyfive-days-ago>7</threesixtyfive-days-ago>
            </stability>
            <stability>
                <name>home_address</name>
                <one-minute-ago>2</one-minute-ago>
                <ten-minutes-ago>2</ten-minutes-ago>
                <one-hour-ago>2</one-hour-ago>
                <twentyfour-hours-ago>2</twentyfour-hours-ago>
                <seven-days-ago>2</seven-days-ago>
                <fifteen-days-ago>2</fifteen-days-ago>
                <thirty-days-ago>2</thirty-days-ago>
                <ninety-days-ago>3</ninety-days-ago>
                <threesixtyfive-days-ago>3</threesixtyfive-days-ago>
            </stability>
            <stability>
                <name>zip_code</name>
                <one-minute-ago>2</one-minute-ago>
                <ten-minutes-ago>2</ten-minutes-ago>
                <one-hour-ago>2</one-hour-ago>
                <twentyfour-hours-ago>2</twentyfour-hours-ago>
                <seven-days-ago>2</seven-days-ago>
                <fifteen-days-ago>2</fifteen-days-ago>
                <thirty-days-ago>2</thirty-days-ago>
                <ninety-days-ago>3</ninety-days-ago>
                <threesixtyfive-days-ago>3</threesixtyfive-days-ago>
            </stability>
            <stability>
                <name>state_address</name>
                <one-minute-ago>1</one-minute-ago>
                <ten-minutes-ago>1</ten-minutes-ago>
                <one-hour-ago>1</one-hour-ago>
                <twentyfour-hours-ago>1</twentyfour-hours-ago>
                <seven-days-ago>1</seven-days-ago>
                <fifteen-days-ago>1</fifteen-days-ago>
                <thirty-days-ago>1</thirty-days-ago>
                <ninety-days-ago>1</ninety-days-ago>
                <threesixtyfive-days-ago>1</threesixtyfive-days-ago>
            </stability>
            <stability>
                <name>home_phone</name>
                <one-minute-ago>2</one-minute-ago>
                <ten-minutes-ago>3</ten-minutes-ago>
                <one-hour-ago>3</one-hour-ago>
                <twentyfour-hours-ago>3</twentyfour-hours-ago>
                <seven-days-ago>3</seven-days-ago>
                <fifteen-days-ago>3</fifteen-days-ago>
                <thirty-days-ago>3</thirty-days-ago>
                <ninety-days-ago>3</ninety-days-ago>
                <threesixtyfive-days-ago>3</threesixtyfive-days-ago>
            </stability>
            <stability>
                <name>cell_phone</name>
                <one-minute-ago>2</one-minute-ago>
                <ten-minutes-ago>2</ten-minutes-ago>
                <one-hour-ago>2</one-hour-ago>
                <twentyfour-hours-ago>2</twentyfour-hours-ago>
                <seven-days-ago>2</seven-days-ago>
                <fifteen-days-ago>2</fifteen-days-ago>
                <thirty-days-ago>3</thirty-days-ago>
                <ninety-days-ago>3</ninety-days-ago>
                <threesixtyfive-days-ago>6</threesixtyfive-days-ago>
            </stability>
            <stability>
                <name>work_phone</name>
                <one-minute-ago>2</one-minute-ago>
                <ten-minutes-ago>2</ten-minutes-ago>
                <one-hour-ago>2</one-hour-ago>
                <twentyfour-hours-ago>3</twentyfour-hours-ago>
                <seven-days-ago>3</seven-days-ago>
                <fifteen-days-ago>3</fifteen-days-ago>
                <thirty-days-ago>3</thirty-days-ago>
                <ninety-days-ago>3</ninety-days-ago>
                <threesixtyfive-days-ago>3</threesixtyfive-days-ago>
            </stability>
            <stability>
                <name>monthly_income</name>
                <one-minute-ago>2</one-minute-ago>
                <ten-minutes-ago>2</ten-minutes-ago>
                <one-hour-ago>3</one-hour-ago>
                <twentyfour-hours-ago>3</twentyfour-hours-ago>
                <seven-days-ago>3</seven-days-ago>
                <fifteen-days-ago>3</fifteen-days-ago>
                <thirty-days-ago>3</thirty-days-ago>
                <ninety-days-ago>3</ninety-days-ago>
                <threesixtyfive-days-ago>3</threesixtyfive-days-ago>
            </stability>
            <stability>
                <name>email_address</name>
                <one-minute-ago>2</one-minute-ago>
                <ten-minutes-ago>2</ten-minutes-ago>
                <one-hour-ago>2</one-hour-ago>
                <twentyfour-hours-ago>3</twentyfour-hours-ago>
                <seven-days-ago>3</seven-days-ago>
                <fifteen-days-ago>3</fifteen-days-ago>
                <thirty-days-ago>3</thirty-days-ago>
                <ninety-days-ago>4</ninety-days-ago>
                <threesixtyfive-days-ago>4</threesixtyfive-days-ago>
            </stability>
        </stabilities>
        <crosstabs>
            <crosstab>
                <name>ssn</name>
                <bank-account>7</bank-account>
                <cell-phone>6</cell-phone>
                <drivers-license>4</drivers-license>
                <email-address>4</email-address>
                <home-address>3</home-address>
                <home-phone>3</home-phone>
                <ssn>1</ssn>
                <zip-code>3</zip-code>
            </crosstab>
            <crosstab>
                <name>drivers_license</name>
                <bank-account>2</bank-account>
                <cell-phone>2</cell-phone>
                <drivers-license>1</drivers-license>
                <email-address>2</email-address>
                <home-address>2</home-address>
                <home-phone>2</home-phone>
                <ssn>1</ssn>
                <zip-code>2</zip-code>
            </crosstab>
            <crosstab>
                <name>bank_account</name>
                <bank-account>1</bank-account>
                <cell-phone>1</cell-phone>
                <drivers-license>1</drivers-license>
                <email-address>1</email-address>
                <home-address>1</home-address>
                <home-phone>1</home-phone>
                <ssn>1</ssn>
                <zip-code>1</zip-code>
            </crosstab>
            <crosstab>
                <name>home_address</name>
                <bank-account>2</bank-account>
                <cell-phone>2</cell-phone>
                <drivers-license>2</drivers-license>
                <email-address>2</email-address>
                <home-address>1</home-address>
                <home-phone>2</home-phone>
                <ssn>1</ssn>
                <zip-code>1</zip-code>
            </crosstab>
            <crosstab>
                <name>home_phone</name>
                <bank-account>1</bank-account>
                <cell-phone>1</cell-phone>
                <drivers-license>1</drivers-license>
                <email-address>1</email-address>
                <home-address>1</home-address>
                <home-phone>1</home-phone>
                <ssn>1</ssn>
                <zip-code>1</zip-code>
            </crosstab>
            <crosstab>
                <name>cell_phone</name>
                <bank-account>1</bank-account>
                <cell-phone>1</cell-phone>
                <drivers-license>1</drivers-license>
                <email-address>1</email-address>
                <home-address>1</home-address>
                <home-phone>1</home-phone>
                <ssn>1</ssn>
                <zip-code>1</zip-code>
            </crosstab>
            <crosstab>
                <name>email_address</name>
                <bank-account>2</bank-account>
                <cell-phone>2</cell-phone>
                <drivers-license>2</drivers-license>
                <email-address>1</email-address>
                <home-address>2</home-address>
                <home-phone>1</home-phone>
                <ssn>1</ssn>
                <zip-code>2</zip-code>
            </crosstab>
        </crosstabs>
        <ratios>
            <ratio>
                <name>one_minute_ago</name>
                <one-minute-ago>1.0</one-minute-ago>
                <ten-minutes-ago>0.85</ten-minutes-ago>
                <one-hour-ago>0.77</one-hour-ago>
                <twentyfour-hours-ago>0.71</twentyfour-hours-ago>
                <seven-days-ago>0.71</seven-days-ago>
                <fifteen-days-ago>0.68</fifteen-days-ago>
                <thirty-days-ago>0.65</thirty-days-ago>
                <ninety-days-ago>0.57</ninety-days-ago>
                <threesixtyfive-days-ago>0.47</threesixtyfive-days-ago>
            </ratio>
            <ratio>
                <name>ten_minutes_ago</name>
                <ten-minutes-ago>1.0</ten-minutes-ago>
                <one-hour-ago>0.91</one-hour-ago>
                <twentyfour-hours-ago>0.83</twentyfour-hours-ago>
                <seven-days-ago>0.83</seven-days-ago>
                <fifteen-days-ago>0.8</fifteen-days-ago>
                <thirty-days-ago>0.77</thirty-days-ago>
                <ninety-days-ago>0.67</ninety-days-ago>
                <threesixtyfive-days-ago>0.56</threesixtyfive-days-ago>
            </ratio>
            <ratio>
                <name>one_hour_ago</name>
                <one-hour-ago>1.0</one-hour-ago>
                <twentyfour-hours-ago>0.92</twentyfour-hours-ago>
                <seven-days-ago>0.92</seven-days-ago>
                <fifteen-days-ago>0.88</fifteen-days-ago>
                <thirty-days-ago>0.85</thirty-days-ago>
                <ninety-days-ago>0.73</ninety-days-ago>
                <threesixtyfive-days-ago>0.61</threesixtyfive-days-ago>
            </ratio>
            <ratio>
                <name>twentyfour_hours_ago</name>
                <twentyfour-hours-ago>1.0</twentyfour-hours-ago>
                <seven-days-ago>1.0</seven-days-ago>
                <fifteen-days-ago>0.96</fifteen-days-ago>
                <thirty-days-ago>0.92</thirty-days-ago>
                <ninety-days-ago>0.8</ninety-days-ago>
                <threesixtyfive-days-ago>0.67</threesixtyfive-days-ago>
            </ratio>
            <ratio>
                <name>seven_days_ago</name>
                <seven-days-ago>1.0</seven-days-ago>
                <fifteen-days-ago>0.96</fifteen-days-ago>
                <thirty-days-ago>0.92</thirty-days-ago>
                <ninety-days-ago>0.8</ninety-days-ago>
                <threesixtyfive-days-ago>0.67</threesixtyfive-days-ago>
            </ratio>
            <ratio>
                <name>fifteen_days_ago</name>
                <fifteen-days-ago>1.0</fifteen-days-ago>
                <thirty-days-ago>0.96</thirty-days-ago>
                <ninety-days-ago>0.83</ninety-days-ago>
                <threesixtyfive-days-ago>0.69</threesixtyfive-days-ago>
            </ratio>
            <ratio>
                <name>thirty_days_ago</name>
                <thirty-days-ago>1.0</thirty-days-ago>
                <ninety-days-ago>0.87</ninety-days-ago>
                <threesixtyfive-days-ago>0.72</threesixtyfive-days-ago>
            </ratio>
            <ratio>
                <name>ninety_days_ago</name>
                <ninety-days-ago>1.0</ninety-days-ago>
                <threesixtyfive-days-ago>0.83</threesixtyfive-days-ago>
            </ratio>
            <ratio>
                <name>threesixtyfive_days_ago</name>
                <threesixtyfive-days-ago>1.0</threesixtyfive-days-ago>
            </ratio>
        </ratios>
        <reason-codes nil="true"/>
        <stability-reason-codes>A104|A102|A103|A105</stability-reason-codes>
    </clear-fraud-insight>
</xml-response>
|]
