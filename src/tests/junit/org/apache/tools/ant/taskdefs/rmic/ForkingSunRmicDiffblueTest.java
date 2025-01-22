package org.apache.tools.ant.taskdefs.rmic;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class ForkingSunRmicDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ForkingSunRmic}
   *   <li>{@link ForkingSunRmic#getExecutableName()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ForkingSunRmic actualForkingSunRmic = new ForkingSunRmic();
    String actualExecutableName = actualForkingSunRmic.getExecutableName();

    // Assert
    assertNull(actualForkingSunRmic.getRmic());
    assertNull(actualForkingSunRmic.getMapper());
    assertEquals(SunRmic.RMIC_EXECUTABLE, actualExecutableName);
  }
}
