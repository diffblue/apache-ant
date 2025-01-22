package org.apache.tools.ant.taskdefs.optional.extension;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class CompatibilityDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Compatibility#Compatibility(String)}
   *   <li>{@link Compatibility#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange, Act and Assert
    assertEquals("Name", (new Compatibility("Name")).toString());
  }
}
