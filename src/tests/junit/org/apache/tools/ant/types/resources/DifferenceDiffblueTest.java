package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.List;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FilterChain;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class DifferenceDiffblueTest {
  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_givenAllButFirstAddNone_thenReturnEmpty() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilterChain {@link FilterChain} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatAddFilterChainFilterChain() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addFilterChain(new FilterChain());
    c.addText("Text");

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertEquals(1, actualCollection.size());
    Resource getResult = ((List<Resource>) actualCollection).get(0);
    assertEquals("Concat$ConcatResource \"concat (Text)\"", getResult.toLongString());
    assertEquals("concat (Text)", getResult.getName());
    assertEquals(-1L, getResult.getSize());
    assertEquals(0L, getResult.getLastModified());
    assertFalse(getResult.isFilesystemOnly());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatProjectIsProject() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setProject(new Project());
    c.addText("Text");

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertEquals(1, actualCollection.size());
    Resource getResult = ((List<Resource>) actualCollection).get(0);
    assertEquals("Concat$ConcatResource \"concat (Text)\"", getResult.toLongString());
    assertEquals("concat (Text)", getResult.getName());
    assertEquals(-1L, getResult.getSize());
    assertEquals(0L, getResult.getLastModified());
    assertFalse(getResult.isFilesystemOnly());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Archives} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_givenDifferenceAddArchives_thenReturnEmpty() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new Archives());
    difference.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Difference} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_givenDifferenceAddDifference_thenThrowBuildException() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new Difference());
    difference.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> difference.getCollection());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_givenDifferenceAddFileResource_thenReturnEmpty() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new FileResource());
    difference.add(new FileResource());

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link FileResource#FileResource()}.</li>
   *   <li>Then return first is {@link FileResource#FileResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_givenDifferenceAddFileResource_thenReturnFirstIsFileResource() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    FileResource c = new FileResource();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertEquals(1, actualCollection.size());
    assertSame(c, ((List<Resource>) actualCollection).get(0));
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Files#Files()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_givenDifferenceAddFiles_thenReturnEmpty() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(new Files());
    difference.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_givenDifferenceAddNone_thenReturnEmpty() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(Resources.NONE);
    difference.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_givenDifferenceAddNone_thenThrowBuildException() throws BuildException {
    // Arrange
    Difference difference = new Difference();
    difference.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> difference.getCollection());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Given {@link Difference} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_givenDifference_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Difference()).getCollection());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Given {@link Files#Files()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return first Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_givenFilesProjectIsProject_thenReturnFirstProjectIsProject() throws BuildException {
    // Arrange
    Files c = new Files();
    Project project = new Project();
    c.setProject(project);
    c.appendIncludes(new String[]{"**"});

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertEquals(1, actualCollection.size());
    Resource getResult = ((List<Resource>) actualCollection).get(0);
    assertTrue(getResult instanceof FileResource);
    assertSame(project, getResult.getProject());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Then return first toLongString is {@code Concat$ConcatResource "concat ()"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_thenReturnFirstToLongStringIsConcatConcatResourceConcat() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addFilelist(new FileList());

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertEquals(1, actualCollection.size());
    Resource getResult = ((List<Resource>) actualCollection).get(0);
    assertEquals("Concat$ConcatResource \"concat ()\"", getResult.toLongString());
    assertEquals("concat ()", getResult.getName());
    assertEquals(-1L, getResult.getSize());
    assertEquals(0L, getResult.getLastModified());
    assertFalse(getResult.isFilesystemOnly());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Then return first toLongString is {@code Concat$ConcatResource "concat ()"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_thenReturnFirstToLongStringIsConcatConcatResourceConcat2() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(new FileList());

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertEquals(1, actualCollection.size());
    Resource getResult = ((List<Resource>) actualCollection).get(0);
    assertEquals("Concat$ConcatResource \"concat ()\"", getResult.toLongString());
    assertEquals("concat ()", getResult.getName());
    assertEquals(-1L, getResult.getSize());
    assertEquals(0L, getResult.getLastModified());
    assertFalse(getResult.isFilesystemOnly());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Then return first toLongString is {@code Concat$ConcatResource "concat ("}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_thenReturnFirstToLongStringIsConcatConcatResourceConcat3() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setResourceName("concat (");
    c.addFilelist(new FileList());

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertEquals(1, actualCollection.size());
    Resource getResult = ((List<Resource>) actualCollection).get(0);
    assertEquals("Concat$ConcatResource \"concat (\"", getResult.toLongString());
    assertEquals("concat (", getResult.getName());
    assertEquals(-1L, getResult.getSize());
    assertEquals(0L, getResult.getLastModified());
    assertFalse(getResult.isFilesystemOnly());
  }

  /**
   * Test {@link Difference#getCollection()}.
   * <ul>
   *   <li>Then return first toLongString is {@code Concat$ConcatResource "concat (Text)"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Difference#getCollection()}
   */
  @Test
  public void testGetCollection_thenReturnFirstToLongStringIsConcatConcatResourceConcatText() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addText("Text");

    Difference difference = new Difference();
    difference.add(c);
    difference.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = difference.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertEquals(1, actualCollection.size());
    Resource getResult = ((List<Resource>) actualCollection).get(0);
    assertEquals("Concat$ConcatResource \"concat (Text)\"", getResult.toLongString());
    assertEquals("concat (Text)", getResult.getName());
    assertEquals(-1L, getResult.getSize());
    assertEquals(0L, getResult.getLastModified());
    assertFalse(getResult.isFilesystemOnly());
  }

  /**
   * Test new {@link Difference} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Difference}
   */
  @Test
  public void testNewDifference() {
    // Arrange and Act
    Difference actualDifference = new Difference();

    // Assert
    Location location = actualDifference.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDifference.getDescription());
    assertNull(actualDifference.getProject());
    assertNull(actualDifference.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(actualDifference.isCache());
  }
}
