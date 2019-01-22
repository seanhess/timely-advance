module Network.Authy where


-- https://api.authy.com/protected/json/sms/verification/start?via=sms&country_code=1&phone_number=8014189376&locale=en
-- X-Authy-API-Key: xxx

-- 200
-- {
--     "carrier": "Google (Grand Central) BWI - Bandwidth.com - SVR",
--     "is_cellphone": false,
--     "message": "Text message sent to +1 801-418-9376.",
--     "seconds_to_expire": 599,
--     "uuid": "b02ce6b0-fd79-0136-c71b-0e93255c39da",
--     "success": true
-- }

-- 400
-- {
--     "error_code": "60033",
--     "message": "Phone number is invalid",
--     "errors": {
--         "message": "Phone number is invalid"
--     },
--     "success": false
-- }


-- https://api.authy.com/protected/json/phones/verification/check?country_code=1&phone_number=8014189376&verification_code=1298
-- X-Authy-API-Key: xxx

-- 200
-- {
--     "message": "Verification code is correct.",
--     "success": true
-- }

-- 401
-- {
--     "error_code": "60022",
--     "message": "Verification code is incorrect",
--     "errors": {
--         "message": "Verification code is incorrect"
--     },
--     "success": false
-- }
