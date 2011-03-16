/*
Copyright Paul James Mutton, 2001-2004, http://www.jibble.org/

This file is part of JBouncer.

This software is dual-licensed, allowing you to choose between the GNU
General Public License (GPL) and the www.jibble.org Commercial License.
Since the GPL may be too restrictive for use in a proprietary application,
a commercial license is also provided. Full license information can be
found at http://www.jibble.org/licenses/

$Author: pjm2 $
$Id: User.java,v 1.2 2004/03/01 19:13:37 pjm2 Exp $

*/

package jbouncer;

public class User {

    public User(String login, String password) {
        this.login = login;
        this.password = password;
    }

    public String getLogin() {
        return login;
    }

    public String getPassword() {
        return password;
    }

    public int hashCode() {
        return login.hashCode() + password.hashCode();
    }

    public boolean equals(Object o) {
        User other = (User) o;
        return other.login.equals(login) && other.password.equals(password);
    }

    private String login;
    private String password;

}