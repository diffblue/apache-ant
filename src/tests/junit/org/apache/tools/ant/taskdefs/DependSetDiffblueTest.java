package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.List;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.Union;
import org.junit.Test;

public class DependSetDiffblueTest {
  /**
   * Test {@link DependSet#createSources()}.
   * <ul>
   *   <li>Given {@link DependSet} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSet#createSources()}
   */
  @Test
  public void testCreateSources_givenDependSet_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Union actualCreateSourcesResult = (new DependSet()).createSources();

    // Assert
    Location location = actualCreateSourcesResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateSourcesResult.getDescription());
    assertNull(actualCreateSourcesResult.getProject());
    assertNull(actualCreateSourcesResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateSourcesResult.size());
    assertFalse(actualCreateSourcesResult.isReference());
    assertTrue(actualCreateSourcesResult.getResourceCollections().isEmpty());
    assertTrue(actualCreateSourcesResult.isEmpty());
    assertTrue(actualCreateSourcesResult.isCache());
  }

  /**
   * Test {@link DependSet#createSources()}.
   * <ul>
   *   <li>Then return ResourceCollections size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSet#createSources()}
   */
  @Test
  public void testCreateSources_thenReturnResourceCollectionsSizeIsOne() {
    // Arrange
    DependSet dependSet = new DependSet();
    FileSet fs = new FileSet();
    dependSet.addSrcfileset(fs);

    // Act and Assert
    List<ResourceCollection> resourceCollections = dependSet.createSources().getResourceCollections();
    assertEquals(1, resourceCollections.size());
    assertSame(fs, resourceCollections.get(0));
  }

  /**
   * Test {@link DependSet#createTargets()}.
   * <ul>
   *   <li>Given {@link DependSet} (default constructor) addTargetfileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSet#createTargets()}
   */
  @Test
  public void testCreateTargets_givenDependSetAddTargetfilesetFileSet() {
    // Arrange
    DependSet dependSet = new DependSet();
    dependSet.addTargetfileset(new FileSet());

    // Act
    Path actualCreateTargetsResult = dependSet.createTargets();

    // Assert
    Location location = actualCreateTargetsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateTargetsResult.getDescription());
    assertNull(actualCreateTargetsResult.getProject());
    assertNull(actualCreateTargetsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCreateTargetsResult.isReference());
  }

  /**
   * Test {@link DependSet#createTargets()}.
   * <ul>
   *   <li>Given {@link DependSet} (default constructor).</li>
   *   <li>Then return size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSet#createTargets()}
   */
  @Test
  public void testCreateTargets_givenDependSet_thenReturnSizeIsZero() {
    // Arrange and Act
    Path actualCreateTargetsResult = (new DependSet()).createTargets();

    // Assert
    Location location = actualCreateTargetsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateTargetsResult.getDescription());
    assertNull(actualCreateTargetsResult.getProject());
    assertNull(actualCreateTargetsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateTargetsResult.size());
    assertFalse(actualCreateTargetsResult.isReference());
    assertTrue(actualCreateTargetsResult.isEmpty());
  }

  /**
   * Test {@link DependSet#addTargetfilelist(FileList)}.
   * <ul>
   *   <li>Given {@link DependSet} (default constructor).</li>
   *   <li>When {@link FileList#FileList()}.</li>
   *   <li>Then {@link FileList#FileList()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSet#addTargetfilelist(FileList)}
   */
  @Test
  public void testAddTargetfilelist_givenDependSet_whenFileList_thenFileListProjectIsNull() {
    // Arrange
    DependSet dependSet = new DependSet();
    FileList fl = new FileList();

    // Act
    dependSet.addTargetfilelist(fl);

    // Assert that nothing has changed
    assertNull(dependSet.getProject());
    assertNull(fl.getProject());
  }

  /**
   * Test {@link DependSet#addTargetfilelist(FileList)}.
   * <ul>
   *   <li>Given {@link DependSet} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link DependSet} (default constructor) Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSet#addTargetfilelist(FileList)}
   */
  @Test
  public void testAddTargetfilelist_givenDependSet_whenNull_thenDependSetProjectIsNull() {
    // Arrange
    DependSet dependSet = new DependSet();

    // Act
    dependSet.addTargetfilelist(null);

    // Assert that nothing has changed
    assertNull(dependSet.getProject());
  }

  /**
   * Test {@link DependSet#addTargetfilelist(FileList)}.
   * <ul>
   *   <li>Then {@link DependSet} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSet#addTargetfilelist(FileList)}
   */
  @Test
  public void testAddTargetfilelist_thenDependSetProjectIsProject() {
    // Arrange
    DependSet dependSet = new DependSet();
    Project project = new Project();
    dependSet.setProject(project);
    dependSet.addTargetfileset(new FileSet());
    FileList fl = new FileList();

    // Act
    dependSet.addTargetfilelist(fl);

    // Assert
    assertSame(project, dependSet.getProject());
    assertSame(project, fl.getProject());
  }

  /**
   * Test {@link DependSet#addTargetfilelist(FileList)}.
   * <ul>
   *   <li>Then {@link FileList#FileList()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSet#addTargetfilelist(FileList)}
   */
  @Test
  public void testAddTargetfilelist_thenFileListProjectIsNull() {
    // Arrange
    DependSet dependSet = new DependSet();
    dependSet.addTargetfileset(new FileSet());
    FileList fl = new FileList();

    // Act
    dependSet.addTargetfilelist(fl);

    // Assert that nothing has changed
    assertNull(dependSet.getProject());
    assertNull(fl.getProject());
  }

  /**
   * Test {@link DependSet#execute()}.
   * <ul>
   *   <li>Given {@link DependSet} (default constructor) addSrcfileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSet#execute()}
   */
  @Test
  public void testExecute_givenDependSetAddSrcfilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    DependSet dependSet = new DependSet();
    dependSet.addSrcfileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> dependSet.execute());
  }

  /**
   * Test {@link DependSet#execute()}.
   * <ul>
   *   <li>Given {@link DependSet} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSet#execute()}
   */
  @Test
  public void testExecute_givenDependSet_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new DependSet()).execute());
  }

  /**
   * Test new {@link DependSet} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DependSet}
   */
  @Test
  public void testNewDependSet() {
    // Arrange and Act
    DependSet actualDependSet = new DependSet();

    // Assert
    assertNull(actualDependSet.getDescription());
    assertNull(actualDependSet.getTaskName());
    assertNull(actualDependSet.getTaskType());
    assertNull(actualDependSet.getProject());
    assertNull(actualDependSet.getOwningTarget());
    assertFalse(actualDependSet.hasSelectors());
  }
}
