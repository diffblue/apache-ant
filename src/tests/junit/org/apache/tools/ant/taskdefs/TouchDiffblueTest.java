package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.junit.Test;

public class TouchDiffblueTest {
  /**
   * Test {@link Touch#addConfiguredMapper(Mapper)}.
   * <ul>
   *   <li>Given {@link Touch} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Touch#addConfiguredMapper(Mapper)}
   */
  @Test
  public void testAddConfiguredMapper_givenTouchAddCutDirsMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    Touch touch = new Touch();
    touch.add(new CutDirsMapper());

    Mapper mapper = new Mapper(new Project());
    mapper.addConfigured(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> touch.addConfiguredMapper(mapper));
  }

  /**
   * Test {@link Touch#add(FileNameMapper)} with {@code fileNameMapper}.
   * <ul>
   *   <li>Given {@link Touch} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Touch#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper_givenTouchAddCutDirsMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    Touch touch = new Touch();
    touch.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> touch.add(new CutDirsMapper()));
  }

  /**
   * Test {@link Touch#checkConfiguration()}.
   * <p>
   * Method under test: {@link Touch#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration() throws BuildException {
    // Arrange
    Touch touch = new Touch();
    touch.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    touch.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> touch.checkConfiguration());
  }

  /**
   * Test {@link Touch#checkConfiguration()}.
   * <ul>
   *   <li>Given {@link Touch} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Touch#checkConfiguration()}
   */
  @Test
  public void testCheckConfiguration_givenTouch_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Touch()).checkConfiguration());
  }

  /**
   * Test {@link Touch#execute()}.
   * <p>
   * Method under test: {@link Touch#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Touch touch = new Touch();
    touch.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    touch.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> touch.execute());
  }

  /**
   * Test {@link Touch#execute()}.
   * <ul>
   *   <li>Given {@link Touch} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Touch#execute()}
   */
  @Test
  public void testExecute_givenTouch_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Touch()).execute());
  }

  /**
   * Test new {@link Touch} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Touch}
   */
  @Test
  public void testNewTouch() {
    // Arrange and Act
    Touch actualTouch = new Touch();

    // Assert
    Location location = actualTouch.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTouch.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualTouch.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualTouch.getTaskName());
    assertNull(actualTouch.getTaskType());
    assertNull(actualTouch.getProject());
    assertNull(actualTouch.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualTouch, runtimeConfigurableWrapper.getProxy());
  }
}
