//
//  AppDelegate.swift
//  Timely
//
//  Created by Sean Hess on 2/14/19.
//  Copyright © 2019 Timely Advance. All rights reserved.
//

import UIKit

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {

    var window: UIWindow?


    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?) -> Bool {
        // Override point for customization after application launch.
        return true
    }

    func applicationWillResignActive(_ application: UIApplication) {
        // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
        // Use this method to pause ongoing tasks, disable timers, and invalidate graphics rendering callbacks. Games should use this method to pause the game.
    }

    func applicationDidEnterBackground(_ application: UIApplication) {
        // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later.
        // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
    }

    func applicationWillEnterForeground(_ application: UIApplication) {
        // Called as part of the transition from the background to the active state; here you can undo many of the changes made on entering the background.
//        print("appWillEnterForeground")
    }

    func applicationDidBecomeActive(_ application: UIApplication) {
        // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
        let vc = self.window?.rootViewController as! ViewController
        print("appDidBecomeActive", vc)
        vc.reloadUI()
        
        // TODO call the server, check the version.
        // Does it match the one we have internally? (This might be a cold load of the app)
        // I can store it in an defatul thing. Some stored thing.
        // https://test.timelyadvance.com/health -- Timely 0.8.c97dbe9a
        
        // I need a way to DO a cold load of the app
        // Download local files.
        // https://test.timelyadvance.com/css/style.css
        // <script src="https://cdn.plaid.com/link/v2/stable/link-initialize.js"></script>
        // <script src="/v1/config.js" type="text/javascript"></script>
        // <script src="js/main.js"></script>

        // Is it just aggressive caching? Can't I fix this with HTTP headers?
        // Probably!
        // Yeah, that's what I should do.
    }

    func applicationWillTerminate(_ application: UIApplication) {
        // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
    }


}

