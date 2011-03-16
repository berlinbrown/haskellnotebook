/*
Copyright Paul James Mutton, 2001-2004, http://www.jibble.org/

This file is part of JBouncer.

This software is dual-licensed, allowing you to choose between the GNU
General Public License (GPL) and the www.jibble.org Commercial License.
Since the GPL may be too restrictive for use in a proprietary application,
a commercial license is also provided. Full license information can be
found at http://www.jibble.org/licenses/

$Author: pjm2 $
$Id: JBouncerManager.java,v 1.2 2004/03/01 19:13:37 pjm2 Exp $

*/

package jbouncer;

import java.io.*;
import java.net.*;
import java.util.*;

public class JBouncerManager {

    public JBouncerManager(HashMap bouncers) {
        this.bouncers = bouncers;
        startTime = System.currentTimeMillis();
    }

    public JBouncer getBouncer(User user) {
        return (JBouncer) bouncers.get(user);
    }

    public HashMap getBouncers() {
        return bouncers;
    }

    public long getStartTime() {
        return startTime;
    }

    public static void log(String message) {
        System.out.println("[" + new Date() + "] " + message);
    }

    // Maps User -> JBouncer
    private HashMap bouncers;

    private long startTime;

}