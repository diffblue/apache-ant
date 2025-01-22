package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.List;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class AllButFirstDiffblueTest {
  /**
   * Test {@link AllButFirst#getCollection()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButFirst#getCollection()}
   */
  @Test
  public void testGetCollection_givenAllButFirstAddNone_thenReturnList() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = allButFirst.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButFirst#getCollection()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButFirst#getCollection()}
   */
  @Test
  public void testGetCollection_givenAllButFirstAddNone_thenReturnList2() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act
    Collection<Resource> actualCollection = allButFirst.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButFirst#getCollection()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButFirst#getCollection()}
   */
  @Test
  public void testGetCollection_givenFirstAddNone_thenReturnList() throws BuildException {
    // Arrange
    First c = new First();
    c.add(Resources.NONE);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act
    Collection<Resource> actualCollection = allButFirst.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link AllButFirst#size()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link AllButFirst} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButFirst#size()}
   */
  @Test
  public void testSize_givenAllButFirstAddAllButFirst_thenReturnZero() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertEquals(0, allButFirst.size());
  }

  /**
   * Test {@link AllButFirst#size()}.
   * <ul>
   *   <li>Given {@link AllButFirst} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButFirst#size()}
   */
  @Test
  public void testSize_givenAllButFirstAddNone_thenReturnZero() throws BuildException {
    // Arrange
    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(Resources.NONE);

    // Act and Assert
    assertEquals(0, allButFirst.size());
  }

  /**
   * Test {@link AllButFirst#size()}.
   * <ul>
   *   <li>Given {@link AllButLast} (default constructor) add {@link AllButFirst} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButFirst#size()}
   */
  @Test
  public void testSize_givenAllButLastAddAllButFirst_thenReturnZero() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    AllButLast c2 = new AllButLast();
    c2.add(c);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c2);

    // Act and Assert
    assertEquals(0, allButFirst.size());
  }

  /**
   * Test {@link AllButFirst#size()}.
   * <ul>
   *   <li>Given {@link AllButLast} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButFirst#size()}
   */
  @Test
  public void testSize_givenAllButLastAddNone_thenReturnZero() throws BuildException {
    // Arrange
    AllButLast c = new AllButLast();
    c.add(Resources.NONE);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertEquals(0, allButFirst.size());
  }

  /**
   * Test {@link AllButFirst#size()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link AllButFirst} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButFirst#size()}
   */
  @Test
  public void testSize_givenFirstAddAllButFirst_thenReturnZero() throws BuildException {
    // Arrange
    AllButFirst c = new AllButFirst();
    c.add(Resources.NONE);

    First c2 = new First();
    c2.add(c);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c2);

    // Act and Assert
    assertEquals(0, allButFirst.size());
  }

  /**
   * Test {@link AllButFirst#size()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link AllButLast} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButFirst#size()}
   */
  @Test
  public void testSize_givenFirstAddAllButLast_thenReturnZero() throws BuildException {
    // Arrange
    AllButLast c = new AllButLast();
    c.add(Resources.NONE);

    First c2 = new First();
    c2.add(c);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c2);

    // Act and Assert
    assertEquals(0, allButFirst.size());
  }

  /**
   * Test {@link AllButFirst#size()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link First} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButFirst#size()}
   */
  @Test
  public void testSize_givenFirstAddFirst_thenReturnZero() throws BuildException {
    // Arrange
    First c = new First();
    c.add(Resources.NONE);

    First c2 = new First();
    c2.add(c);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c2);

    // Act and Assert
    assertEquals(0, allButFirst.size());
  }

  /**
   * Test {@link AllButFirst#size()}.
   * <ul>
   *   <li>Given {@link First} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AllButFirst#size()}
   */
  @Test
  public void testSize_givenFirstAddNone_thenReturnZero() throws BuildException {
    // Arrange
    First c = new First();
    c.add(Resources.NONE);

    AllButFirst allButFirst = new AllButFirst();
    allButFirst.add(c);

    // Act and Assert
    assertEquals(0, allButFirst.size());
  }

  /**
   * Test new {@link AllButFirst} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AllButFirst}
   */
  @Test
  public void testNewAllButFirst() {
    // Arrange and Act
    AllButFirst actualAllButFirst = new AllButFirst();

    // Assert
    Location location = actualAllButFirst.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAllButFirst.getDescription());
    assertNull(actualAllButFirst.getProject());
    assertNull(actualAllButFirst.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(1, actualAllButFirst.getCount());
    assertTrue(actualAllButFirst.isCache());
  }
}
