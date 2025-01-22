package org.apache.tools.ant.util.optional;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.security.AccessControlContext;
import java.security.AllPermission;
import java.security.Permission;
import org.apache.tools.ant.ExitException;
import org.junit.Test;

public class NoExitSecurityManagerDiffblueTest {
  /**
   * Test {@link NoExitSecurityManager#checkExit(int)}.
   * <p>
   * Method under test: {@link NoExitSecurityManager#checkExit(int)}
   */
  @Test
  public void testCheckExit() {
    // Arrange, Act and Assert
    assertThrows(ExitException.class, () -> (new NoExitSecurityManager()).checkExit(1));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link NoExitSecurityManager}
   *   <li>{@link NoExitSecurityManager#checkPermission(Permission)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    NoExitSecurityManager actualNoExitSecurityManager = new NoExitSecurityManager();
    actualNoExitSecurityManager.checkPermission(new AllPermission("foo", "foo"));

    // Assert
    Object securityContext = actualNoExitSecurityManager.getSecurityContext();
    assertTrue(securityContext instanceof AccessControlContext);
    assertNull(((AccessControlContext) securityContext).getDomainCombiner());
  }
}
