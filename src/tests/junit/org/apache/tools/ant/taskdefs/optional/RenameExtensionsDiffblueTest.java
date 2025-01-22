package org.apache.tools.ant.taskdefs.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class RenameExtensionsDiffblueTest {
  /**
   * Test new {@link RenameExtensions} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link RenameExtensions}
   */
  @Test
  public void testNewRenameExtensions() {
    // Arrange and Act
    RenameExtensions actualRenameExtensions = new RenameExtensions();

    // Assert
    Location location = actualRenameExtensions.getLocation();
    assertNull(location.getFileName());
    assertNull(actualRenameExtensions.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualRenameExtensions.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualRenameExtensions.getTaskName());
    assertNull(actualRenameExtensions.getTaskType());
    assertNull(actualRenameExtensions.getProject());
    assertNull(actualRenameExtensions.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualRenameExtensions.hasSelectors());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualRenameExtensions, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test {@link RenameExtensions#execute()}.
   * <ul>
   *   <li>Given {@link RenameExtensions} (default constructor) FromExtension is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RenameExtensions#execute()}
   */
  @Test
  public void testExecute_givenRenameExtensionsFromExtensionIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    RenameExtensions renameExtensions = new RenameExtensions();
    renameExtensions.setFromExtension(null);
    renameExtensions.setToExtension(null);
    renameExtensions.setSrcDir(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> renameExtensions.execute());
  }

  /**
   * Test {@link RenameExtensions#execute()}.
   * <ul>
   *   <li>Given {@link RenameExtensions} (default constructor) ToExtension is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RenameExtensions#execute()}
   */
  @Test
  public void testExecute_givenRenameExtensionsToExtensionIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    RenameExtensions renameExtensions = new RenameExtensions();
    renameExtensions.setFromExtension("foo");
    renameExtensions.setToExtension(null);
    renameExtensions.setSrcDir(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> renameExtensions.execute());
  }

  /**
   * Test {@link RenameExtensions#execute()}.
   * <ul>
   *   <li>Given {@link RenameExtensions} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RenameExtensions#execute()}
   */
  @Test
  public void testExecute_givenRenameExtensions_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new RenameExtensions()).execute());
  }
}
