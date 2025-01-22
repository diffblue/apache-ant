package org.apache.tools.ant.taskdefs.compilers;

import static org.junit.Assert.assertNull;
import org.junit.Test;

public class SjDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Sj}
   *   <li>{@link Sj#getNoDebugArgument()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    Sj actualSj = new Sj();

    // Assert
    assertNull(actualSj.getNoDebugArgument());
    assertNull(actualSj.getProject());
    assertNull(actualSj.getJavac());
  }
}
