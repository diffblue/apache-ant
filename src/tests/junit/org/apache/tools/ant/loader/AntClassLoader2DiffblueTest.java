package org.apache.tools.ant.loader;

import static org.junit.Assert.assertNotNull;
import org.junit.Test;

public class AntClassLoader2DiffblueTest {
  /**
   * Test new {@link AntClassLoader2} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AntClassLoader2}
   */
  @Test
  public void testNewAntClassLoader2() {
    // Arrange, Act and Assert
    assertNotNull(new AntClassLoader2());
  }
}
