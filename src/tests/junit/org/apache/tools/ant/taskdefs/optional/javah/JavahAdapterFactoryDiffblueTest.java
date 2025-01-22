package org.apache.tools.ant.taskdefs.optional.javah;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class JavahAdapterFactoryDiffblueTest {
  /**
   * Test {@link JavahAdapterFactory#getDefault()}.
   * <p>
   * Method under test: {@link JavahAdapterFactory#getDefault()}
   */
  @Test
  public void testGetDefault() {
    // Arrange, Act and Assert
    assertEquals(ForkingJavah.IMPLEMENTATION_NAME, JavahAdapterFactory.getDefault());
  }
}
