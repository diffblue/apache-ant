package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.Set;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class IntersectDiffblueTest {
  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenAllButFirstAddNone_thenReturnEmpty() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    Intersect intersect = new Intersect();
    intersect.add(c);
    intersect.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = intersect.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatAddFilelistFileList_thenReturnEmpty() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addFilelist(new FileList());

    Intersect intersect = new Intersect();
    intersect.add(c);
    intersect.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = intersect.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code Text}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatAddTextText_thenReturnEmpty() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addText("Text");

    Intersect intersect = new Intersect();
    intersect.add(c);
    intersect.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = intersect.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatDestIsResource_thenReturnEmpty() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(new FileList());

    Intersect intersect = new Intersect();
    intersect.add(c);
    intersect.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = intersect.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatProjectIsProject_thenReturnEmpty() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setProject(new Project());
    c.addText("Text");

    Intersect intersect = new Intersect();
    intersect.add(c);
    intersect.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = intersect.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) ResourceName is {@code concat (}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatResourceNameIsConcat_thenReturnEmpty() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setResourceName("concat (");
    c.addFilelist(new FileList());

    Intersect intersect = new Intersect();
    intersect.add(c);
    intersect.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = intersect.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Intersect} (default constructor) add {@link Archives} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenIntersectAddArchives_thenReturnEmpty() throws BuildException {
    // Arrange
    Intersect intersect = new Intersect();
    intersect.add(new Archives());
    intersect.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = intersect.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Intersect} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenIntersectAddFileResource_thenReturnEmpty() throws BuildException {
    // Arrange
    Intersect intersect = new Intersect();
    intersect.add(new FileResource());
    intersect.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = intersect.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Intersect} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenIntersectAddFileResource_thenReturnSizeIsOne() throws BuildException {
    // Arrange
    Intersect intersect = new Intersect();
    intersect.add(new FileResource());
    intersect.add(new FileResource());

    // Act
    Collection<Resource> actualCollection = intersect.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertEquals(1, actualCollection.size());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Intersect} (default constructor) add {@link Files#Files()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenIntersectAddFiles_thenReturnEmpty() throws BuildException {
    // Arrange
    Intersect intersect = new Intersect();
    intersect.add(new Files());
    intersect.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = intersect.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Intersect} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenIntersectAddNone_thenReturnEmpty() throws BuildException {
    // Arrange
    Intersect intersect = new Intersect();
    intersect.add(Resources.NONE);
    intersect.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = intersect.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Intersect} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenIntersectAddNone_thenReturnEmpty2() throws BuildException {
    // Arrange
    Intersect intersect = new Intersect();
    intersect.add(Resources.NONE);
    intersect.add(Resources.NONE);
    intersect.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = intersect.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Intersect} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenIntersectAddNone_thenThrowBuildException() throws BuildException {
    // Arrange
    Intersect intersect = new Intersect();
    intersect.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> intersect.getCollection());
  }

  /**
   * Test {@link Intersect#getCollection()}.
   * <ul>
   *   <li>Given {@link Intersect} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Intersect#getCollection()}
   */
  @Test
  public void testGetCollection_givenIntersect_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Intersect()).getCollection());
  }

  /**
   * Test new {@link Intersect} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Intersect}
   */
  @Test
  public void testNewIntersect() {
    // Arrange and Act
    Intersect actualIntersect = new Intersect();

    // Assert
    Location location = actualIntersect.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIntersect.getDescription());
    assertNull(actualIntersect.getProject());
    assertNull(actualIntersect.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(actualIntersect.isCache());
  }
}
