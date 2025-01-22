package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class EqualsDiffblueTest {
  /**
   * Test {@link Equals#eval()}.
   * <p>
   * Method under test: {@link Equals#eval()}
   */
  @Test
  public void testEval() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Equals()).eval());
  }
}
