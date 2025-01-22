package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.Ant.TargetElement;
import org.apache.tools.ant.types.DirSet;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Path.PathElement;
import org.junit.Test;

public class SubAntDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SubAnt#setAntfile(String)}
   *   <li>{@link SubAnt#setFailonerror(boolean)}
   *   <li>{@link SubAnt#setGenericAntfile(File)}
   *   <li>{@link SubAnt#setInheritall(boolean)}
   *   <li>{@link SubAnt#setInheritrefs(boolean)}
   *   <li>{@link SubAnt#setOutput(String)}
   *   <li>{@link SubAnt#setTarget(String)}
   *   <li>{@link SubAnt#setVerbose(boolean)}
   *   <li>{@link SubAnt#getDefaultBuildFile()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    SubAnt subAnt = new SubAnt();

    // Act
    subAnt.setAntfile("Antfile");
    subAnt.setFailonerror(true);
    subAnt.setGenericAntfile(Copy.NULL_FILE_PLACEHOLDER);
    subAnt.setInheritall(true);
    subAnt.setInheritrefs(true);
    subAnt.setOutput("foo");
    subAnt.setTarget("Target");
    subAnt.setVerbose(true);

    // Assert
    assertEquals("build.xml", subAnt.getDefaultBuildFile());
  }

  /**
   * Test {@link SubAnt#handleInput(byte[], int, int)}.
   * <ul>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#handleInput(byte[], int, int)}
   */
  @Test
  public void testHandleInput_thenReturnThree() throws IOException {
    // Arrange
    Project project = new Project();
    project.setDefaultInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    SubAnt subAnt = new SubAnt();
    subAnt.setProject(project);

    // Act and Assert
    assertEquals(3, subAnt.handleInput("AXAXAXAX".getBytes("UTF-8"), 2, 3));
    byte[] byteArray = new byte[5];
    assertEquals(5, subAnt.getProject().getDefaultInputStream().read(byteArray));
    assertArrayEquals("XAXAX".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link SubAnt#execute()}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#execute()}
   */
  @Test
  public void testExecute_givenSubAnt_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SubAnt()).execute());
  }

  /**
   * Test {@link SubAnt#addConfiguredTarget(TargetElement)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addConfiguredTarget(TargetElement)}
   */
  @Test
  public void testAddConfiguredTarget_givenEmptyString_thenThrowBuildException() {
    // Arrange
    SubAnt subAnt = new SubAnt();

    TargetElement t = new TargetElement();
    t.setName("");

    // Act and Assert
    assertThrows(BuildException.class, () -> subAnt.addConfiguredTarget(t));
  }

  /**
   * Test {@link SubAnt#addDirset(DirSet)}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor) addDirset {@link DirSet#DirSet()}.</li>
   *   <li>When {@link DirSet#DirSet()}.</li>
   *   <li>Then {@link DirSet#DirSet()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addDirset(DirSet)}
   */
  @Test
  public void testAddDirset_givenSubAntAddDirsetDirSet_whenDirSet_thenDirSetProjectIsNull() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    subAnt.addDirset(new DirSet());
    DirSet set = new DirSet();

    // Act
    subAnt.addDirset(set);

    // Assert that nothing has changed
    assertNull(subAnt.getProject());
    assertNull(set.getProject());
  }

  /**
   * Test {@link SubAnt#addDirset(DirSet)}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When {@link DirSet#DirSet()}.</li>
   *   <li>Then {@link SubAnt} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addDirset(DirSet)}
   */
  @Test
  public void testAddDirset_givenSubAntProjectIsProject_whenDirSet_thenSubAntProjectIsProject() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    Project project = new Project();
    subAnt.setProject(project);
    DirSet set = new DirSet();

    // Act
    subAnt.addDirset(set);

    // Assert
    assertSame(project, subAnt.getProject());
    assertSame(project, set.getProject());
  }

  /**
   * Test {@link SubAnt#addDirset(DirSet)}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor).</li>
   *   <li>When {@link DirSet#DirSet()}.</li>
   *   <li>Then {@link DirSet#DirSet()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addDirset(DirSet)}
   */
  @Test
  public void testAddDirset_givenSubAnt_whenDirSet_thenDirSetProjectIsNull() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    DirSet set = new DirSet();

    // Act
    subAnt.addDirset(set);

    // Assert that nothing has changed
    assertNull(subAnt.getProject());
    assertNull(set.getProject());
  }

  /**
   * Test {@link SubAnt#addDirset(DirSet)}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link SubAnt} (default constructor) Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addDirset(DirSet)}
   */
  @Test
  public void testAddDirset_givenSubAnt_whenNull_thenSubAntProjectIsNull() {
    // Arrange
    SubAnt subAnt = new SubAnt();

    // Act
    subAnt.addDirset(null);

    // Assert that nothing has changed
    assertNull(subAnt.getProject());
  }

  /**
   * Test {@link SubAnt#addFileset(FileSet)}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor) addDirset {@link DirSet#DirSet()}.</li>
   *   <li>When {@link FileSet#FileSet()}.</li>
   *   <li>Then {@link FileSet#FileSet()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset_givenSubAntAddDirsetDirSet_whenFileSet_thenFileSetProjectIsNull() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    subAnt.addDirset(new DirSet());
    FileSet set = new FileSet();

    // Act
    subAnt.addFileset(set);

    // Assert that nothing has changed
    assertNull(subAnt.getProject());
    assertNull(set.getProject());
  }

  /**
   * Test {@link SubAnt#addFileset(FileSet)}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link SubAnt} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset_givenSubAntProjectIsProject_thenSubAntProjectIsProject() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    Project project = new Project();
    subAnt.setProject(project);
    FileSet set = new FileSet();

    // Act
    subAnt.addFileset(set);

    // Assert
    assertSame(project, subAnt.getProject());
    assertSame(project, set.getProject());
  }

  /**
   * Test {@link SubAnt#addFileset(FileSet)}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor).</li>
   *   <li>When {@link FileSet#FileSet()}.</li>
   *   <li>Then {@link FileSet#FileSet()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset_givenSubAnt_whenFileSet_thenFileSetProjectIsNull() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    FileSet set = new FileSet();

    // Act
    subAnt.addFileset(set);

    // Assert that nothing has changed
    assertNull(subAnt.getProject());
    assertNull(set.getProject());
  }

  /**
   * Test {@link SubAnt#addFileset(FileSet)}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link SubAnt} (default constructor) Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset_givenSubAnt_whenNull_thenSubAntProjectIsNull() {
    // Arrange
    SubAnt subAnt = new SubAnt();

    // Act
    subAnt.addFileset(null);

    // Assert that nothing has changed
    assertNull(subAnt.getProject());
  }

  /**
   * Test {@link SubAnt#addFilelist(FileList)}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor) addDirset {@link DirSet#DirSet()}.</li>
   *   <li>Then {@link FileList#FileList()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addFilelist(FileList)}
   */
  @Test
  public void testAddFilelist_givenSubAntAddDirsetDirSet_thenFileListProjectIsNull() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    subAnt.addDirset(new DirSet());
    FileList list = new FileList();

    // Act
    subAnt.addFilelist(list);

    // Assert that nothing has changed
    assertNull(subAnt.getProject());
    assertNull(list.getProject());
  }

  /**
   * Test {@link SubAnt#addFilelist(FileList)}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link SubAnt} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addFilelist(FileList)}
   */
  @Test
  public void testAddFilelist_givenSubAntProjectIsProject_thenSubAntProjectIsProject() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    Project project = new Project();
    subAnt.setProject(project);
    FileList list = new FileList();

    // Act
    subAnt.addFilelist(list);

    // Assert
    assertSame(project, subAnt.getProject());
    assertSame(project, list.getProject());
  }

  /**
   * Test {@link SubAnt#addFilelist(FileList)}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor).</li>
   *   <li>When {@link FileList#FileList()}.</li>
   *   <li>Then {@link FileList#FileList()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addFilelist(FileList)}
   */
  @Test
  public void testAddFilelist_givenSubAnt_whenFileList_thenFileListProjectIsNull() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    FileList list = new FileList();

    // Act
    subAnt.addFilelist(list);

    // Assert that nothing has changed
    assertNull(subAnt.getProject());
    assertNull(list.getProject());
  }

  /**
   * Test {@link SubAnt#addFilelist(FileList)}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link SubAnt} (default constructor) Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#addFilelist(FileList)}
   */
  @Test
  public void testAddFilelist_givenSubAnt_whenNull_thenSubAntProjectIsNull() {
    // Arrange
    SubAnt subAnt = new SubAnt();

    // Act
    subAnt.addFilelist(null);

    // Assert that nothing has changed
    assertNull(subAnt.getProject());
  }

  /**
   * Test {@link SubAnt#createBuildpath()}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor) addDirset {@link DirSet#DirSet()}.</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#createBuildpath()}
   */
  @Test
  public void testCreateBuildpath_givenSubAntAddDirsetDirSet_thenReturnLocationFileNameIsNull() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    subAnt.addDirset(new DirSet());

    // Act
    Path actualCreateBuildpathResult = subAnt.createBuildpath();

    // Assert
    Location location = actualCreateBuildpathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateBuildpathResult.getDescription());
    assertNull(actualCreateBuildpathResult.getProject());
    assertNull(actualCreateBuildpathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateBuildpathResult.size());
    assertFalse(actualCreateBuildpathResult.isReference());
    assertTrue(actualCreateBuildpathResult.isEmpty());
  }

  /**
   * Test {@link SubAnt#createBuildpath()}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#createBuildpath()}
   */
  @Test
  public void testCreateBuildpath_givenSubAntProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    Project project = new Project();
    subAnt.setProject(project);

    // Act and Assert
    assertSame(project, subAnt.createBuildpath().getProject());
  }

  /**
   * Test {@link SubAnt#createBuildpath()}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#createBuildpath()}
   */
  @Test
  public void testCreateBuildpath_givenSubAnt_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateBuildpathResult = (new SubAnt()).createBuildpath();

    // Assert
    Location location = actualCreateBuildpathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateBuildpathResult.getDescription());
    assertNull(actualCreateBuildpathResult.getProject());
    assertNull(actualCreateBuildpathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateBuildpathResult.size());
    assertFalse(actualCreateBuildpathResult.isReference());
    assertTrue(actualCreateBuildpathResult.isEmpty());
  }

  /**
   * Test {@link SubAnt#createBuildpathElement()}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#createBuildpathElement()}
   */
  @Test
  public void testCreateBuildpathElement_givenSubAnt() {
    // Arrange and Act
    PathElement actualCreateBuildpathElementResult = (new SubAnt()).createBuildpathElement();

    // Assert
    assertNull(actualCreateBuildpathElementResult.getParts());
    assertEquals(0, actualCreateBuildpathElementResult.size());
    assertTrue(actualCreateBuildpathElementResult.isFilesystemOnly());
    assertTrue(actualCreateBuildpathElementResult.isEmpty());
  }

  /**
   * Test {@link SubAnt#createBuildpathElement()}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor) addDirset {@link DirSet#DirSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#createBuildpathElement()}
   */
  @Test
  public void testCreateBuildpathElement_givenSubAntAddDirsetDirSet() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    subAnt.addDirset(new DirSet());

    // Act
    PathElement actualCreateBuildpathElementResult = subAnt.createBuildpathElement();

    // Assert
    assertNull(actualCreateBuildpathElementResult.getParts());
    assertEquals(0, actualCreateBuildpathElementResult.size());
    assertTrue(actualCreateBuildpathElementResult.isFilesystemOnly());
    assertTrue(actualCreateBuildpathElementResult.isEmpty());
  }

  /**
   * Test {@link SubAnt#createBuildpathElement()}.
   * <ul>
   *   <li>Given {@link SubAnt} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SubAnt#createBuildpathElement()}
   */
  @Test
  public void testCreateBuildpathElement_givenSubAntProjectIsProject() {
    // Arrange
    SubAnt subAnt = new SubAnt();
    subAnt.setProject(new Project());

    // Act
    PathElement actualCreateBuildpathElementResult = subAnt.createBuildpathElement();

    // Assert
    assertNull(actualCreateBuildpathElementResult.getParts());
    assertEquals(0, actualCreateBuildpathElementResult.size());
    assertTrue(actualCreateBuildpathElementResult.isFilesystemOnly());
    assertTrue(actualCreateBuildpathElementResult.isEmpty());
  }

  /**
   * Test new {@link SubAnt} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SubAnt}
   */
  @Test
  public void testNewSubAnt() {
    // Arrange and Act
    SubAnt actualSubAnt = new SubAnt();

    // Assert
    assertEquals("build.xml", actualSubAnt.getDefaultBuildFile());
    Location location = actualSubAnt.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSubAnt.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualSubAnt.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualSubAnt.getTaskName());
    assertNull(actualSubAnt.getTaskType());
    assertNull(actualSubAnt.getProject());
    assertNull(actualSubAnt.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualSubAnt, runtimeConfigurableWrapper.getProxy());
  }
}
