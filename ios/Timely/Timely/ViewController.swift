//
//  ViewController.swift
//  Timely
//
//  Created by Sean Hess on 2/14/19.
//  Copyright © 2019 Timely Advance. All rights reserved.
//

import UIKit
import WebKit

class ViewController: UIViewController, WKUIDelegate, WKNavigationDelegate {

    @IBOutlet weak var webView: WKWebView!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        webView.uiDelegate = self
        webView.navigationDelegate = self
        print("viewDidLoad")
        loadUI()
    }
    
    public func loadUI() {
        let url = URL(string: "https://test.timelyadvance.com/")!
        let request = URLRequest(url: url)
        print("LOAD", request)
        webView.load(request)
    }
    
// This doesn't really reload. Actually, re-running doesn't even reload
//    public func reloadUI() {
//        if (!webView.isLoading) {
//            print("RELOAD")
//            webView.reloadFromOrigin()
//        }
//    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        print("didFinish")
    }
    
    func webView(_ webView: WKWebView, didCommit navigation: WKNavigation!) {
        print("didCommit")
    }
    
    func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!) {
        print("didStartProvisional")
    }

    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
        print("didFail")
    }
    
    func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
        print("didFail Providiona", error)
    }

}

