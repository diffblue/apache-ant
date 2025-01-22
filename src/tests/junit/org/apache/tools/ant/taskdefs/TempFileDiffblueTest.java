package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class TempFileDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TempFile#setCreateFile(boolean)}
   *   <li>{@link TempFile#setDeleteOnExit(boolean)}
   *   <li>{@link TempFile#setDestDir(File)}
   *   <li>{@link TempFile#setPrefix(String)}
   *   <li>{@link TempFile#setProperty(String)}
   *   <li>{@link TempFile#setSuffix(String)}
   *   <li>{@link TempFile#isCreateFile()}
   *   <li>{@link TempFile#isDeleteOnExit()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    TempFile tempFile = new TempFile();

    // Act
    tempFile.setCreateFile(true);
    tempFile.setDeleteOnExit(true);
    tempFile.setDestDir(Copy.NULL_FILE_PLACEHOLDER);
    tempFile.setPrefix("Prefix");
    tempFile.setProperty("Property");
    tempFile.setSuffix("Suffix");
    boolean actualIsCreateFileResult = tempFile.isCreateFile();

    // Assert
    assertTrue(actualIsCreateFileResult);
    assertTrue(tempFile.isDeleteOnExit());
  }

  /**
   * Test {@link TempFile#execute()}.
   * <ul>
   *   <li>Given {@link TempFile} (default constructor) Property is empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TempFile#execute()}
   */
  @Test
  public void testExecute_givenTempFilePropertyIsEmptyString_thenThrowBuildException() throws BuildException {
    // Arrange
    TempFile tempFile = new TempFile();
    tempFile.setProperty("");
    tempFile.setDestDir(null);
    tempFile.setProject(null);
    tempFile.setDeleteOnExit(false);
    tempFile.setCreateFile(false);

    // Act and Assert
    assertThrows(BuildException.class, () -> tempFile.execute());
  }

  /**
   * Test {@link TempFile#execute()}.
   * <ul>
   *   <li>Given {@link TempFile} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TempFile#execute()}
   */
  @Test
  public void testExecute_givenTempFile_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TempFile()).execute());
  }

  /**
   * Test new {@link TempFile} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TempFile}
   */
  @Test
  public void testNewTempFile() {
    // Arrange and Act
    TempFile actualTempFile = new TempFile();

    // Assert
    Location location = actualTempFile.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTempFile.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualTempFile.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualTempFile.getTaskName());
    assertNull(actualTempFile.getTaskType());
    assertNull(actualTempFile.getProject());
    assertNull(actualTempFile.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualTempFile.isCreateFile());
    assertFalse(actualTempFile.isDeleteOnExit());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualTempFile, runtimeConfigurableWrapper.getProxy());
  }
}
