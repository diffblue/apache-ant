package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.List;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class AllButLastDiffblueTest {
  /**
   * Test {@link AllButLast#getCollection()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#getCollection()}
   */
  @Test
  public void testGetCollection_givenAllButFirstAddNone_thenReturnList() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c);

    // Act
    Collection<Resource> actualCollection = allButLast.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButLast#getCollection()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#getCollection()}
   */
  @Test
  public void testGetCollection_givenAllButFirstAddNone_thenReturnList2() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    AllButLast c2 = new AllButLast();
    c2.add(c);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c2);

    // Act
    Collection<Resource> actualCollection = allButLast.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButLast#getCollection()}.
   * <ul>
   *   <li>Given {@link AllButLast} (default constructor) add {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#getCollection()}
   */
  @Test
  public void testGetCollection_givenAllButLastAddBZip2Resource_thenReturnList() throws BuildException {
    // Arrange
    AllButLast allButLast = new AllButLast();
    allButLast.add(new BZip2Resource());

    // Act
    Collection<Resource> actualCollection = allButLast.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButLast#getCollection()}.
   * <ul>
   *   <li>Given {@link AllButLast} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#getCollection()}
   */
  @Test
  public void testGetCollection_givenAllButLastAddNone_thenReturnList() throws BuildException {
    // Arrange
    AllButLast allButLast = new AllButLast();
    allButLast.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = allButLast.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButLast#getCollection()}.
   * <ul>
   *   <li>Given {@link AllButLast} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#getCollection()}
   */
  @Test
  public void testGetCollection_givenAllButLastAddNone_thenReturnList2() throws BuildException {
    // Arrange
    AllButLast c = new AllButLast();
    c.add(Resources.NONE);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c);

    // Act
    Collection<Resource> actualCollection = allButLast.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButLast#getCollection()}.
   * <ul>
   *   <li>Given {@link AllButLast} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#getCollection()}
   */
  @Test
  public void testGetCollection_givenAllButLastAddNone_thenReturnList3() throws BuildException {
    // Arrange
    AllButLast c = new AllButLast();
    c.add(Resources.NONE);

    AllButLast c2 = new AllButLast();
    c2.add(c);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c2);

    // Act
    Collection<Resource> actualCollection = allButLast.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButLast#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFileset {@code null}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatAddFilesetNull_thenReturnList() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFileset(null);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c);

    // Act
    Collection<Resource> actualCollection = allButLast.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButLast#getCollection()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#getCollection()}
   */
  @Test
  public void testGetCollection_givenFirstAddBZip2Resource_thenReturnList() throws BuildException {
    // Arrange
    First c = new First();
    c.add(new BZip2Resource());

    AllButLast allButLast = new AllButLast();
    allButLast.add(c);

    // Act
    Collection<Resource> actualCollection = allButLast.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButLast#getCollection()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#getCollection()}
   */
  @Test
  public void testGetCollection_givenFirstAddNone_thenReturnList() throws BuildException {
    // Arrange
    First c = new First();
    c.add(Resources.NONE);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c);

    // Act
    Collection<Resource> actualCollection = allButLast.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButLast#getCollection()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#getCollection()}
   */
  @Test
  public void testGetCollection_givenFirstAddNone_thenReturnList2() throws BuildException {
    // Arrange
    First c = new First();
    c.add(Resources.NONE);

    AllButLast c2 = new AllButLast();
    c2.add(c);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c2);

    // Act
    Collection<Resource> actualCollection = allButLast.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButLast#size()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link AllButLast} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#size()}
   */
  @Test
  public void testSize_givenAllButFirstAddAllButLast_thenReturnZero() throws BuildException {
    // Arrange
    AllButLast c = new AllButLast();
    c.add(Resources.NONE);

    AllButFirst c2 = new AllButFirst();
    c2.add(c);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c2);

    // Act and Assert
    assertEquals(0, allButLast.size());
  }

  /**
   * Test {@link AllButLast#size()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#size()}
   */
  @Test
  public void testSize_givenAllButFirstAddNone_thenReturnZero() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c);

    // Act and Assert
    assertEquals(0, allButLast.size());
  }

  /**
   * Test {@link AllButLast#size()}.
   * <ul>
   *   <li>Given {@link AllButLast} (default constructor) add {@link AllButLast} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#size()}
   */
  @Test
  public void testSize_givenAllButLastAddAllButLast_thenReturnZero() throws BuildException {
    // Arrange
    AllButLast c = new AllButLast();
    c.add(Resources.NONE);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c);

    // Act and Assert
    assertEquals(0, allButLast.size());
  }

  /**
   * Test {@link AllButLast#size()}.
   * <ul>
   *   <li>Given {@link AllButLast} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#size()}
   */
  @Test
  public void testSize_givenAllButLastAddNone_thenReturnZero() throws BuildException {
    // Arrange
    AllButLast allButLast = new AllButLast();
    allButLast.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, allButLast.size());
  }

  /**
   * Test {@link AllButLast#size()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link AllButFirst} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#size()}
   */
  @Test
  public void testSize_givenFirstAddAllButFirst_thenReturnZero() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    First c2 = new First();
    c2.add(c);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c2);

    // Act and Assert
    assertEquals(0, allButLast.size());
  }

  /**
   * Test {@link AllButLast#size()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link AllButLast} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#size()}
   */
  @Test
  public void testSize_givenFirstAddAllButLast_thenReturnZero() throws BuildException {
    // Arrange
    AllButLast c = new AllButLast();
    c.add(Resources.NONE);

    First c2 = new First();
    c2.add(c);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c2);

    // Act and Assert
    assertEquals(0, allButLast.size());
  }

  /**
   * Test {@link AllButLast#size()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link First} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#size()}
   */
  @Test
  public void testSize_givenFirstAddFirst_thenReturnZero() throws BuildException {
    // Arrange
    First c = new First();
    c.add(Resources.NONE);

    First c2 = new First();
    c2.add(c);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c2);

    // Act and Assert
    assertEquals(0, allButLast.size());
  }

  /**
   * Test {@link AllButLast#size()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButLast#size()}
   */
  @Test
  public void testSize_givenFirstAddNone_thenReturnZero() throws BuildException {
    // Arrange
    First c = new First();
    c.add(Resources.NONE);

    AllButLast allButLast = new AllButLast();
    allButLast.add(c);

    // Act and Assert
    assertEquals(0, allButLast.size());
  }

  /**
   * Test new {@link AllButLast} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AllButLast}
   */
  @Test
  public void testNewAllButLast() {
    // Arrange and Act
    AllButLast actualAllButLast = new AllButLast();

    // Assert
    Location location = actualAllButLast.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAllButLast.getDescription());
    assertNull(actualAllButLast.getProject());
    assertNull(actualAllButLast.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(1, actualAllButLast.getCount());
    assertTrue(actualAllButLast.isCache());
  }
}
