package org.apache.tools.ant.taskdefs.optional.windows;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class AttribDiffblueTest {
  /**
   * Test new {@link Attrib} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Attrib}
   */
  @Test
  public void testNewAttrib() {
    // Arrange and Act
    Attrib actualAttrib = new Attrib();

    // Assert
    assertNull(actualAttrib.getDescription());
    assertNull(actualAttrib.getTaskName());
    assertNull(actualAttrib.getTaskType());
    assertNull(actualAttrib.getOs());
    assertNull(actualAttrib.getOsFamily());
    assertNull(actualAttrib.getProject());
    assertNull(actualAttrib.getOwningTarget());
    assertFalse(actualAttrib.getResolveExecutable());
    assertFalse(actualAttrib.isValidOs());
  }

  /**
   * Test {@link Attrib#checkConfiguration()}.
   * <p>
   * Method under test: {@link Attrib#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Attrib()).checkConfiguration());
  }

  /**
   * Test {@link Attrib#setAddsourcefile(boolean)}.
   * <p>
   * Method under test: {@link Attrib#setAddsourcefile(boolean)}
   */
  @Test
  public void testSetAddsourcefile() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Attrib()).setAddsourcefile(true));
  }

  /**
   * Test {@link Attrib#isValidOs()}.
   * <ul>
   *   <li>Given {@link Attrib} (default constructor) OsFamily is {@code windows}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attrib#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenAttribOsFamilyIsWindows_thenReturnFalse() {
    // Arrange
    Attrib attrib = new Attrib();
    attrib.setOsFamily("windows");

    // Act and Assert
    assertFalse(attrib.isValidOs());
  }

  /**
   * Test {@link Attrib#isValidOs()}.
   * <ul>
   *   <li>Given {@link Attrib} (default constructor) Os is {@code windows}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attrib#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenAttribOsIsWindows_thenReturnFalse() {
    // Arrange
    Attrib attrib = new Attrib();
    attrib.setOs("windows");

    // Act and Assert
    assertFalse(attrib.isValidOs());
  }

  /**
   * Test {@link Attrib#isValidOs()}.
   * <ul>
   *   <li>Given {@link Attrib} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attrib#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenAttrib_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Attrib()).isValidOs());
  }
}
