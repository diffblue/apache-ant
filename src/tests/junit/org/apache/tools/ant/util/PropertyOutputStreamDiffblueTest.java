package org.apache.tools.ant.util;

import static org.junit.Assert.assertArrayEquals;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class PropertyOutputStreamDiffblueTest {
  /**
   * Test {@link PropertyOutputStream#PropertyOutputStream(Project, String)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyOutputStream#PropertyOutputStream(Project, String)}
   */
  @Test
  public void testNewPropertyOutputStream_whenProject() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{}, (new PropertyOutputStream(new Project(), "foo")).toByteArray());
  }

  /**
   * Test {@link PropertyOutputStream#PropertyOutputStream(Project, String, boolean)}.
   * <ul>
   *   <li>When {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyOutputStream#PropertyOutputStream(Project, String, boolean)}
   */
  @Test
  public void testNewPropertyOutputStream_whenTrue() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{}, (new PropertyOutputStream(new Project(), "foo", true)).toByteArray());
  }
}
