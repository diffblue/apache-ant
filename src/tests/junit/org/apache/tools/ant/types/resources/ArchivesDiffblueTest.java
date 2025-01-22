package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.Set;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Tar;
import org.apache.tools.ant.taskdefs.Tar.TarFileSet;
import org.apache.tools.ant.types.ArchiveFileSet;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.TarScanner;
import org.junit.Test;

public class ArchivesDiffblueTest {
  /**
   * Test {@link Archives#createZips()}.
   * <p>
   * Method under test: {@link Archives#createZips()}
   */
  @Test
  public void testCreateZips() {
    // Arrange and Act
    Union actualCreateZipsResult = (new Archives()).createZips();

    // Assert
    Collection<String> allToStrings = actualCreateZipsResult.getAllToStrings();
    assertTrue(allToStrings instanceof Set);
    Collection<Resource> collection = actualCreateZipsResult.getCollection();
    assertTrue(collection instanceof Set);
    assertNull(actualCreateZipsResult.getDescription());
    assertNull(actualCreateZipsResult.getProject());
    assertNull(actualCreateZipsResult.getRefid());
    assertEquals(0, actualCreateZipsResult.size());
    assertFalse(actualCreateZipsResult.isReference());
    assertTrue(allToStrings.isEmpty());
    assertTrue(collection.isEmpty());
    assertTrue(actualCreateZipsResult.getResourceCollections().isEmpty());
    assertTrue(actualCreateZipsResult.getAllResources().isEmpty());
    assertTrue(actualCreateZipsResult.isEmpty());
    assertTrue(actualCreateZipsResult.isCache());
  }

  /**
   * Test {@link Archives#createTars()}.
   * <p>
   * Method under test: {@link Archives#createTars()}
   */
  @Test
  public void testCreateTars() {
    // Arrange and Act
    Union actualCreateTarsResult = (new Archives()).createTars();

    // Assert
    Collection<String> allToStrings = actualCreateTarsResult.getAllToStrings();
    assertTrue(allToStrings instanceof Set);
    Collection<Resource> collection = actualCreateTarsResult.getCollection();
    assertTrue(collection instanceof Set);
    assertNull(actualCreateTarsResult.getDescription());
    assertNull(actualCreateTarsResult.getProject());
    assertNull(actualCreateTarsResult.getRefid());
    assertEquals(0, actualCreateTarsResult.size());
    assertFalse(actualCreateTarsResult.isReference());
    assertTrue(allToStrings.isEmpty());
    assertTrue(collection.isEmpty());
    assertTrue(actualCreateTarsResult.getResourceCollections().isEmpty());
    assertTrue(actualCreateTarsResult.getAllResources().isEmpty());
    assertTrue(actualCreateTarsResult.isEmpty());
    assertTrue(actualCreateTarsResult.isCache());
  }

  /**
   * Test {@link Archives#size()}.
   * <p>
   * Method under test: {@link Archives#size()}
   */
  @Test
  public void testSize() {
    // Arrange, Act and Assert
    assertEquals(0, (new Archives()).size());
  }

  /**
   * Test {@link Archives#iterator()}.
   * <p>
   * Method under test: {@link Archives#iterator()}
   */
  @Test
  public void testIterator() {
    // Arrange, Act and Assert
    assertFalse((new Archives()).iterator().hasNext());
  }

  /**
   * Test {@link Archives#isFilesystemOnly()}.
   * <p>
   * Method under test: {@link Archives#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly() {
    // Arrange, Act and Assert
    assertFalse((new Archives()).isFilesystemOnly());
  }

  /**
   * Test {@link Archives#setRefid(Reference)}.
   * <p>
   * Method under test: {@link Archives#setRefid(Reference)}
   */
  @Test
  public void testSetRefid() {
    // Arrange
    Archives archives = new Archives();
    Reference r = new Reference("42");

    // Act
    archives.setRefid(r);

    // Assert
    assertTrue(archives.isReference());
    assertSame(r, archives.getRefid());
  }

  /**
   * Test {@link Archives#clone()}.
   * <p>
   * Method under test: {@link Archives#clone()}
   */
  @Test
  public void testClone() {
    // Arrange and Act
    Object actualCloneResult = (new Archives()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof Archives);
    Location location = ((Archives) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Archives) actualCloneResult).getDescription());
    assertNull(((Archives) actualCloneResult).getProject());
    assertNull(((Archives) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, ((Archives) actualCloneResult).size());
    assertFalse(((Archives) actualCloneResult).isReference());
    assertTrue(((Archives) actualCloneResult).isEmpty());
  }

  /**
   * Test {@link Archives#grabArchives()}.
   * <p>
   * Method under test: {@link Archives#grabArchives()}
   */
  @Test
  public void testGrabArchives() {
    // Arrange, Act and Assert
    assertFalse((new Archives()).grabArchives().hasNext());
  }

  /**
   * Test {@link Archives#configureArchive(ArchiveFileSet, Resource)}.
   * <ul>
   *   <li>Given {@link Archives} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@link Tar.TarFileSet}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Archives#configureArchive(ArchiveFileSet, Resource)}
   */
  @Test
  public void testConfigureArchive_givenArchivesProjectIsProject_thenReturnTarFileSet() {
    // Arrange
    Archives archives = new Archives();
    Project project = new Project();
    archives.setProject(project);
    TarFileSet afs = new TarFileSet();

    // Act
    ArchiveFileSet actualConfigureArchiveResult = archives.configureArchive(afs, new Resource());

    // Assert
    assertTrue(actualConfigureArchiveResult instanceof TarFileSet);
    assertTrue(afs.getDirectoryScanner() instanceof TarScanner);
    assertTrue(actualConfigureArchiveResult.getDirectoryScanner() instanceof TarScanner);
    assertSame(project, afs.getProject());
    assertSame(project, actualConfigureArchiveResult.getProject());
  }

  /**
   * Test {@link Archives#configureArchive(ArchiveFileSet, Resource)}.
   * <ul>
   *   <li>Given {@link Archives} (default constructor).</li>
   *   <li>When {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Archives#configureArchive(ArchiveFileSet, Resource)}
   */
  @Test
  public void testConfigureArchive_givenArchives_whenTarFileSet_thenTarFileSetProjectIsNull() {
    // Arrange
    Archives archives = new Archives();
    TarFileSet afs = new TarFileSet();

    // Act
    ArchiveFileSet actualConfigureArchiveResult = archives.configureArchive(afs, new Resource());

    // Assert
    assertNull(afs.getProject());
    assertFalse(afs.isFilesystemOnly());
    assertSame(afs, actualConfigureArchiveResult);
  }

  /**
   * Test new {@link Archives} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Archives}
   */
  @Test
  public void testNewArchives() {
    // Arrange and Act
    Archives actualArchives = new Archives();

    // Assert
    Location location = actualArchives.getLocation();
    assertNull(location.getFileName());
    assertNull(actualArchives.getDescription());
    assertNull(actualArchives.getProject());
    assertNull(actualArchives.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualArchives.size());
    assertFalse(actualArchives.isReference());
    assertTrue(actualArchives.isEmpty());
  }
}
