package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.junit.Test;

public class ContainerMapperDiffblueTest {
  /**
  * Method under test: {@link ContainerMapper#add(FileNameMapper)}
  */
  @Test
  public void testAdd() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    // Act
    chainedMapper.add(new CutDirsMapper());

    // Assert
    assertEquals(1, chainedMapper.getMappers().size());
  }

  /**
   * Method under test: {@link ContainerMapper#add(FileNameMapper)}
   */
  @Test
  public void testAdd2() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    // Act
    chainedMapper.add(new ChainedMapper());

    // Assert
    assertEquals(1, chainedMapper.getMappers().size());
  }

  /**
   * Method under test: {@link ContainerMapper#add(FileNameMapper)}
   */
  @Test
  public void testAdd3() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    CompositeMapper compositeMapper = new CompositeMapper();
    compositeMapper.addConfigured(new CutDirsMapper());

    // Act
    chainedMapper.add(compositeMapper);

    // Assert
    assertEquals(1, chainedMapper.getMappers().size());
  }

  /**
   * Method under test: {@link ContainerMapper#add(FileNameMapper)}
   */
  @Test
  public void testAdd4() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    CompositeMapper compositeMapper = new CompositeMapper();
    compositeMapper.addConfigured(new ChainedMapper());

    // Act
    chainedMapper.add(compositeMapper);

    // Assert
    assertEquals(1, chainedMapper.getMappers().size());
  }

  /**
   * Method under test: {@link ContainerMapper#addConfigured(FileNameMapper)}
   */
  @Test
  public void testAddConfigured() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    // Act
    chainedMapper.addConfigured(new CutDirsMapper());

    // Assert
    assertEquals(1, chainedMapper.getMappers().size());
  }

  /**
   * Method under test: {@link ContainerMapper#addConfigured(FileNameMapper)}
   */
  @Test
  public void testAddConfigured2() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    // Act
    chainedMapper.addConfigured(new ChainedMapper());

    // Assert
    assertEquals(1, chainedMapper.getMappers().size());
  }

  /**
   * Method under test: {@link ContainerMapper#addConfigured(FileNameMapper)}
   */
  @Test
  public void testAddConfigured3() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    CompositeMapper compositeMapper = new CompositeMapper();
    compositeMapper.addConfigured(new CutDirsMapper());

    // Act
    chainedMapper.addConfigured(compositeMapper);

    // Assert
    assertEquals(1, chainedMapper.getMappers().size());
  }

  /**
   * Method under test: {@link ContainerMapper#addConfigured(FileNameMapper)}
   */
  @Test
  public void testAddConfigured4() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    CompositeMapper compositeMapper = new CompositeMapper();
    compositeMapper.addConfigured(new ChainedMapper());

    // Act
    chainedMapper.addConfigured(compositeMapper);

    // Assert
    assertEquals(1, chainedMapper.getMappers().size());
  }

  /**
   * Method under test: {@link ContainerMapper#contains(FileNameMapper)}
   */
  @Test
  public void testContains() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    // Act and Assert
    assertFalse(chainedMapper.contains(new CutDirsMapper()));
  }

  /**
   * Method under test: {@link ContainerMapper#contains(FileNameMapper)}
   */
  @Test
  public void testContains2() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    chainedMapper.addConfigured(new CutDirsMapper());

    // Act and Assert
    assertFalse(chainedMapper.contains(new CutDirsMapper()));
  }

  /**
   * Method under test: {@link ContainerMapper#contains(FileNameMapper)}
   */
  @Test
  public void testContains3() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    chainedMapper.addConfigured(new ChainedMapper());

    // Act and Assert
    assertFalse(chainedMapper.contains(new CutDirsMapper()));
  }

  /**
   * Method under test: {@link ContainerMapper#getMappers()}
   */
  @Test
  public void testGetMappers() {
    // Arrange, Act and Assert
    assertTrue((new ChainedMapper()).getMappers().isEmpty());
  }

  /**
   * Method under test: {@link ContainerMapper#setFrom(String)}
   */
  @Test
  public void testSetFrom() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    // Act
    chainedMapper.setFrom("Ignore");

    // Assert that nothing has changed
    assertTrue(chainedMapper.getMappers().isEmpty());
  }

  /**
   * Method under test: {@link ContainerMapper#setTo(String)}
   */
  @Test
  public void testSetTo() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();

    // Act
    chainedMapper.setTo("Ignore");

    // Assert that nothing has changed
    assertTrue(chainedMapper.getMappers().isEmpty());
  }
}

