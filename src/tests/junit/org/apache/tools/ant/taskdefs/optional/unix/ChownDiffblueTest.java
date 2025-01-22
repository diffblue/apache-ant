package org.apache.tools.ant.taskdefs.optional.unix;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class ChownDiffblueTest {
  /**
   * Test new {@link Chown} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Chown}
   */
  @Test
  public void testNewChown() {
    // Arrange and Act
    Chown actualChown = new Chown();

    // Assert
    assertNull(actualChown.getDescription());
    assertNull(actualChown.getTaskName());
    assertNull(actualChown.getTaskType());
    assertNull(actualChown.getOs());
    assertNull(actualChown.getOsFamily());
    assertNull(actualChown.getProject());
    assertNull(actualChown.getOwningTarget());
    assertFalse(actualChown.getResolveExecutable());
    assertTrue(actualChown.isValidOs());
  }

  /**
   * Test {@link Chown#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Chown} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Chown#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenChown_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Chown()).checkConfiguration());
  }

  /**
   * Test {@link Chown#setExecutable(String)}.
   * <p>
   * Method under test: {@link Chown#setExecutable(String)}
   */
  @Test
  public void testSetExecutable() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Chown()).setExecutable("foo"));
  }
}
