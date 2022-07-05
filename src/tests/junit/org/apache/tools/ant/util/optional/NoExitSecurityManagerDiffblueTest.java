package org.apache.tools.ant.util.optional;

import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.ExitException;
import org.junit.Test;

public class NoExitSecurityManagerDiffblueTest {
  /**
  * Method under test: {@link NoExitSecurityManager#checkExit(int)}
  */
  @Test
  public void testCheckExit() {
    // Arrange, Act and Assert
    assertThrows(ExitException.class, () -> (new NoExitSecurityManager()).checkExit(1));
  }
}

