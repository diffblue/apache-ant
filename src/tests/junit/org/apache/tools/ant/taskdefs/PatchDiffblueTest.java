package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class PatchDiffblueTest {
  /**
   * Test {@link Patch#setPatchfile(File)}.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Patch#setPatchfile(File)}
   */
  @Test
  public void testSetPatchfile_whenNull_file_placeholder_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Patch()).setPatchfile(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Patch#setStrip(int)}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Patch#setStrip(int)}
   */
  @Test
  public void testSetStrip_whenMinusOne_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Patch()).setStrip(-1));
  }

  /**
   * Test {@link Patch#execute()}.
   * <ul>
   *   <li>Given {@link Patch} (default constructor) Dir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Patch#execute()}
   */
  @Test
  public void testExecute_givenPatchDirIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    Patch patch = new Patch();
    patch.setDir(Copy.NULL_FILE_PLACEHOLDER);
    patch.setPatchfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> patch.execute());
  }

  /**
   * Test {@link Patch#execute()}.
   * <ul>
   *   <li>Given {@link Patch} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Patch#execute()}
   */
  @Test
  public void testExecute_givenPatch_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Patch()).execute());
  }

  /**
   * Test new {@link Patch} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Patch}
   */
  @Test
  public void testNewPatch() {
    // Arrange and Act
    Patch actualPatch = new Patch();

    // Assert
    Location location = actualPatch.getLocation();
    assertNull(location.getFileName());
    assertNull(actualPatch.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualPatch.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualPatch.getTaskName());
    assertNull(actualPatch.getTaskType());
    assertNull(actualPatch.getProject());
    assertNull(actualPatch.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualPatch, runtimeConfigurableWrapper.getProxy());
  }
}
