package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class MkdirDiffblueTest {
  /**
   * Test {@link Mkdir#execute()}.
   * <ul>
   *   <li>Given {@link Mkdir} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mkdir#execute()}
   */
  @Test
  public void testExecute_givenMkdir_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Mkdir()).execute());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Mkdir#setDir(File)}
   *   <li>{@link Mkdir#getDir()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Mkdir mkdir = new Mkdir();
    File dir = Copy.NULL_FILE_PLACEHOLDER;

    // Act
    mkdir.setDir(dir);

    // Assert
    assertSame(dir, mkdir.getDir());
  }

  /**
   * Test new {@link Mkdir} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Mkdir}
   */
  @Test
  public void testNewMkdir() {
    // Arrange and Act
    Mkdir actualMkdir = new Mkdir();

    // Assert
    assertNull(actualMkdir.getDir());
    Location location = actualMkdir.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMkdir.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualMkdir.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualMkdir.getTaskName());
    assertNull(actualMkdir.getTaskType());
    assertNull(actualMkdir.getProject());
    assertNull(actualMkdir.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualMkdir, runtimeConfigurableWrapper.getProxy());
  }
}
